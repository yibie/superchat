;;; superchat-tape-view.el — Tape views for Superchat -*- lexical-binding: t; -*-

;;; Commentary:

;; View functions over the SQLite tape table.  Views are runtime
;; collections of entries; they do not introduce new storage structures.
;; This follows the tape.systems primitive model:
;;   Entry (immutable) → Anchor (marked distillation) → View (collection).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'superchat-projection)

(declare-function superchat-db-tape-select  "superchat-db" (sql &optional params))
(declare-function superchat-db-tape-search   "superchat-db" (query &optional session-id limit))
(declare-function superchat-db-tape-replay   "superchat-db" (session-id &optional since-id limit))
(declare-function superchat-db-tape-last-anchor "superchat-db" (session-id))

;; ═══════════════════════════════════════════════════════════
;; Low-level view constructors
;; ═══════════════════════════════════════════════════════════

(defun superchat-view-entries (session-id &optional since-id kinds limit)
  "Return tape entries for SESSION-ID.
SINCE-ID restricts to entries with id > SINCE-ID.
KINDS is a list of kinds to include (e.g. '(\"user\" \"anchor\")).
LIMIT caps the result count."
  (let* ((kind-clause
          (when kinds
            (format "AND kind IN (%s)"
                    (mapconcat (lambda (k) (format "'%s'" k)) kinds ","))))
         (limit-clause (when limit (format "LIMIT %d" limit)))
         (sql (format "SELECT id, topic, kind, content, meta, created_at
                       FROM tape
                       WHERE session_id = ? %s
                       ORDER BY id ASC
                       %s"
                      (or kind-clause "")
                      (or limit-clause "")))
         (params (list session-id)))
    (when since-id
      (setq sql (replace-regexp-in-string "session_id = \\?"
                                          "session_id = ? AND id > ?"
                                          sql t t))
      (setq params (list session-id since-id)))
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

(defun superchat-view-session-history (session-id &optional max-id seen)
  "Return SESSION-ID history, following branch lineage through MAX-ID.
SEEN is an internal cycle guard."
  (when (member session-id seen)
    (error "Cyclic Superchat session lineage at %s" session-id))
  (let* ((rows (superchat-view-entries session-id))
         (rows (if max-id
                   (cl-remove-if (lambda (row) (> (nth 0 row) max-id)) rows)
                 rows))
         (parent (superchat-view--branch-parent rows)))
    (if parent
        (append
         (superchat-view-session-history
          (car parent) (cdr parent) (cons session-id seen))
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
        :authority-policy "default"
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
      ("authority.policy.selected"
       (when-let* ((policy
                    (superchat-view--object-get
                     payload '(authority_policy authority-policy policy))))
         (setq config (plist-put config :authority-policy policy))))
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
      ("intent.received"
       (when-let* ((surface
                    (superchat-view--object-get payload '(surface))))
         (setq config (plist-put config :surface surface)))))
    config))

(superchat-projection-register
 'session-configuration
 :version 1
 :initial #'superchat-view--session-config-initial
 :apply #'superchat-view--session-config-apply)

(defun superchat-view-session-configuration (session-id)
  "Replay SESSION-ID history into the latest runtime configuration plist."
  (superchat-projection-replay
   'session-configuration
   (superchat-view-session-history session-id)))

(defun superchat-view-anchor-source-ids (anchor-row)
  "Extract source entry ids from an ANCHOR-ROW's meta JSON."
  (when anchor-row
    (let* ((meta (condition-case nil
                     (json-read-from-string (or (nth 3 anchor-row) "{}"))
                   (error (make-hash-table :test 'equal))))
           (payload (or (superchat-view--object-get meta '(payload))
                        meta)))
      (superchat-view--object-get payload '(source_ids)))))

(defun superchat-view-expand-anchor (session-id anchor-row)
  "Return the entries that ANCHOR-ROW distills for SESSION-ID.
If the anchor has no recorded source ids, fall back to all entries
before the anchor."
  (when anchor-row
    (let* ((anchor-id (nth 0 anchor-row))
           (source-ids (superchat-view-anchor-source-ids anchor-row))
           (ids (cond
                 ((vectorp source-ids) (append source-ids nil))
                 ((listp source-ids) source-ids)
                 (t nil))))
      (if ids
          (superchat-db-tape-select
           (format "SELECT id, topic, kind, content, meta, created_at
                    FROM tape
                    WHERE session_id = ? AND id IN (%s)
                    ORDER BY id ASC"
                   (mapconcat (lambda (_) "?") ids ","))
           (cons session-id ids))
        (superchat-view-entries session-id nil nil 200)))))

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
  (let* ((rows (superchat-view-entries session-id))
         (rows (if max-id
                   (cl-remove-if (lambda (row) (> (nth 0 row) max-id)) rows)
                 rows))
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

(defun superchat-view--continuity-raw-item (record)
  "Normalize a material continuity RECORD as a raw fact."
  (list :class 'raw-fact
        :session-id (superchat-view--continuity-record-session-id record)
        :storage-id (superchat-view--continuity-record-id record)
        :kind (superchat-view--continuity-record-kind record)
        :content (superchat-view--continuity-record-content record)))

(defun superchat-view--continuity-role (kind)
  "Return the canonical prompt role label for material KIND."
  (cond
   ((string= kind "user") "User")
   ((string= kind "assistant") "Assistant")
   ((string= kind "system") "System")
   (t (error "Unsupported continuity prompt kind: %s" kind))))

(defun superchat-view--continuity-item-line (item)
  "Render one normalized continuity ITEM as its canonical body line."
  (format "%s: %s"
          (superchat-view--continuity-role (plist-get item :kind))
          (plist-get item :content)))

(defun superchat-view--continuity-recall-item-line (item)
  "Render any explicitly recalled ITEM without treating it as prompt material."
  (format "%s: %s"
          (capitalize
           (replace-regexp-in-string "_" " " (plist-get item :kind)))
          (plist-get item :content)))

(defun superchat-view--continuity-select-records
    (records entry-budget char-budget &optional line-function)
  "Select a recent suffix of RECORDS within ENTRY-BUDGET and CHAR-BUDGET.
LINE-FUNCTION renders a normalized item for character accounting."
  (let ((render-line (or line-function
                         #'superchat-view--continuity-item-line))
        (selected nil)
        (selected-count 0)
        (selected-chars 0)
        boundary-record
        boundary-reason)
    (catch 'budget-full
      (dolist (record (reverse (copy-sequence records)))
        (let* ((item (superchat-view--continuity-raw-item record))
               (line-length (length (funcall render-line item)))
               (candidate-chars
                (+ selected-chars line-length
                   (if (> selected-count 0) 1 0))))
          (when (>= selected-count entry-budget)
            (setq boundary-record record)
            (setq boundary-reason 'budget)
            (throw 'budget-full nil))
          (when (> candidate-chars char-budget)
            (setq boundary-record record)
            (setq boundary-reason
                  (if (and (> char-budget 0)
                           (> line-length char-budget))
                      'oversized
                    'budget))
            (throw 'budget-full nil))
          (push record selected)
          (setq selected-count (1+ selected-count))
          (setq selected-chars candidate-chars))))
    (list :records selected
          :boundary-record boundary-record
          :boundary-reason boundary-reason)))

(defun superchat-view--continuity-material-record-p (record)
  "Return non-nil when continuity RECORD is prompt material."
  (let ((kind (superchat-view--continuity-record-kind record)))
    (or (member kind '("user" "assistant"))
        (and (string= kind "system")
             (not (superchat-view--continuity-runtime-event-p record))))))

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

(defun superchat-view--continuity-missing-ranges
    (session-id lo hi present-ids)
  "Return `missing-source' omissions in [LO, HI] absent from PRESENT-IDS."
  (let ((cursor lo)
        ranges)
    (dolist (id present-ids)
      (when (> id cursor)
        (push (list :session-id session-id
                    :range (vector cursor (1- id))
                    :reason 'missing-source
                    :recoverable 'no)
              ranges))
      (setq cursor (1+ id)))
    (when (<= cursor hi)
      (push (list :session-id session-id
                  :range (vector cursor hi)
                  :reason 'missing-source
                  :recoverable 'no)
            ranges))
    (vconcat (nreverse ranges))))

(defun superchat-view--continuity-id-omission-ranges
    (session-id ids reason recoverable)
  "Compress storage IDS into omission ranges for SESSION-ID."
  (let ((sorted (sort (delete-dups (copy-sequence ids)) #'<))
        start previous ranges)
    (dolist (id sorted)
      (if (and previous (= id (1+ previous)))
          (setq previous id)
        (when start
          (push (list :session-id session-id
                      :range (vector start previous)
                      :reason reason
                      :recoverable recoverable)
                ranges))
        (setq start id)
        (setq previous id)))
    (when start
      (push (list :session-id session-id
                  :range (vector start previous)
                  :reason reason
                  :recoverable recoverable)
            ranges))
    (vconcat (nreverse ranges))))

(defun superchat-view--continuity-range-row-count (ranges)
  "Return the number of storage positions represented by RANGES."
  (let ((count 0))
    (dolist (range (append ranges nil) count)
      (let ((bounds (plist-get range :range)))
        (setq count (+ count 1 (- (aref bounds 1) (aref bounds 0))))))))

(defun superchat-view--continuity-merge-ranges (&rest vectors)
  "Merge disclosure range VECTORS in chronological storage order."
  (vconcat
   (sort (apply #'append (mapcar (lambda (vector) (append vector nil))
                                  vectors))
         (lambda (left right)
           (< (aref (plist-get left :range) 0)
              (aref (plist-get right :range) 0))))))

(defun superchat-view--continuity-range-label (range)
  "Render one disclosure RANGE as a canonical raw segment label."
  (let* ((session-id (plist-get range :session-id))
         (bounds (plist-get range :range))
         (first-id (aref bounds 0))
         (last-id (aref bounds 1)))
    (if (= first-id last-id)
        (format "%s#%d" session-id first-id)
      (format "%s#%d-%d" session-id first-id last-id))))

(defun superchat-view--continuity-prompt
    (items covered omitted-count recall-available &optional line-function)
  "Render canonical prompt for ITEMS, COVERED, omissions, and recall state."
  (let* ((render-line (or line-function
                          #'superchat-view--continuity-item-line))
         (has-items (> (length items) 0))
         (segment (if has-items
                      (mapconcat #'superchat-view--continuity-range-label
                                 (append covered nil) ", ")
                    "none"))
         (header (format
                  "[Context v0 | raw %s | represented none | omitted %s | recall %s]"
                  segment
                  (if (> omitted-count 0)
                      (number-to-string omitted-count)
                    "none")
                  (if recall-available "available" "none")))
         (body
          (mapconcat
           render-line
           (append items nil)
           "\n")))
    (if has-items
        (concat header "\n\n" body)
      header)))

(cl-defun superchat-view--continuity-assemble
    (session-id &key (purpose 'prompt) max-entries max-chars)
  "Assemble a deterministic context continuity envelope for SESSION-ID."
  (let* ((segments
          (superchat-view--continuity-lineage-segments session-id))
         (all-records
          (superchat-view--continuity-segment-records segments))
         (material-records
          (cl-remove-if-not #'superchat-view--continuity-material-record-p
                            all-records))
         (entry-budget (or max-entries 50))
         (char-budget (or max-chars 4000))
         (selection
          (superchat-view--continuity-select-records
           material-records entry-budget char-budget))
         (records (plist-get selection :records))
         (boundary-record (plist-get selection :boundary-record))
         (boundary-reason (plist-get selection :boundary-reason))
         (selected-ids
          (mapcar #'superchat-view--continuity-record-id records))
         (classified-omissions
          (delq
           nil
           (mapcar
            (lambda (record)
              (cond
               ((not (superchat-view--continuity-material-record-p record))
                (cons record
                      (superchat-view--continuity-nonmaterial-reason record)))
               ((member (superchat-view--continuity-record-id record)
                        selected-ids)
                nil)
               ((and (eq boundary-reason 'oversized)
                     (= (superchat-view--continuity-record-id record)
                        (superchat-view--continuity-record-id
                         boundary-record)))
                (cons record 'oversized))
               (t (cons record 'budget))))
            all-records)))
         (omitted-count (length classified-omissions))
         (resource-omitted-p
          (cl-some (lambda (entry)
                     (memq (cdr entry)
                           '(budget oversized unsupported-schema)))
                   classified-omissions))
         (items (vconcat
                 (mapcar #'superchat-view--continuity-raw-item records)))
         (tape-head
          (and all-records
               (superchat-view--continuity-record-id
                (car (last all-records)))))
         (covered (superchat-view--continuity-record-ranges records))
         (omissions (superchat-view--continuity-omission-ranges
                     classified-omissions))
         (recallable (superchat-view--continuity-recallable-ranges
                      classified-omissions)))
    (list :contract "context-continuity-v0"
          :status (cond
                   ((null all-records) 'empty)
                   (resource-omitted-p 'partial)
                   (t 'satisfied))
          :prompt (if (null all-records)
                      ""
                    (superchat-view--continuity-prompt
                     items covered omitted-count (> (length recallable) 0)))
          :scope (list :session-id session-id
                       :purpose purpose
                       :through-storage-id tape-head
                       :lineage
                       (superchat-view--continuity-public-lineage segments)
                       :selection-policy superchat-view--continuity-policy)
          :items items
          :disclosure
          (list :covered covered
                :represented []
                :omitted omissions
                :recallable recallable)
          :receipt (list :tape-head tape-head
                         :read-status 'ok
                         :selection-policy superchat-view--continuity-policy
                         :selection-policy-version 0))))

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
                               :selection-policy-version 0))))
    (if recall
        (append envelope (list :recall recall))
      envelope)))

(cl-defun superchat-view-assemble
    (session-id &key (purpose 'prompt) max-entries max-chars
                after-storage-id through-storage-id pinned-storage-ids)
  "Assemble a deterministic context envelope, containing read failures."
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
  (when (or after-storage-id
            through-storage-id
            (and pinned-storage-ids (> (length pinned-storage-ids) 0)))
    (error "Continuity window and pin selectors are reserved, not implemented"))
  (condition-case nil
      (superchat-view--continuity-assemble
       session-id
       :purpose purpose
       :max-entries max-entries
       :max-chars max-chars)
    (error (superchat-view--continuity-unknown-envelope session-id purpose))))

(defun superchat-view--continuity-recall-envelope
    (session-id selector reason through-storage-id tape-head candidate-records
                max-entries max-chars)
  "Build one bounded recall envelope from CANDIDATE-RECORDS."
  (let* ((selection
          (superchat-view--continuity-select-records
           candidate-records (or max-entries 50) (or max-chars 4000)
           #'superchat-view--continuity-recall-item-line))
         (records (plist-get selection :records))
         (boundary-record (plist-get selection :boundary-record))
         (boundary-reason (plist-get selection :boundary-reason))
         (selected-ids
          (mapcar #'superchat-view--continuity-record-id records))
         (classified-omissions
          (delq
           nil
           (mapcar
            (lambda (record)
              (cond
               ((member (superchat-view--continuity-record-id record)
                        selected-ids)
                nil)
               ((and (eq boundary-reason 'oversized)
                     (= (superchat-view--continuity-record-id record)
                        (superchat-view--continuity-record-id boundary-record)))
                (cons record 'oversized))
               (t (cons record 'budget))))
            candidate-records)))
         (items (vconcat
                 (mapcar #'superchat-view--continuity-raw-item records)))
         (covered (superchat-view--continuity-record-ranges records))
         (omissions (superchat-view--continuity-omission-ranges
                     classified-omissions))
         (recallable (superchat-view--continuity-recallable-ranges
                      classified-omissions))
         (omitted-count (length classified-omissions)))
    (list :contract "context-continuity-v0"
          :status (if classified-omissions 'partial 'satisfied)
          :prompt (superchat-view--continuity-prompt
                   items covered omitted-count (> (length recallable) 0)
                   #'superchat-view--continuity-recall-item-line)
          :scope (list :session-id session-id
                       :purpose 'recall
                       :through-storage-id through-storage-id
                       :lineage
                       (vector
                        (list :session-id session-id
                              :through-storage-id through-storage-id))
                       :selection-policy
                       superchat-view--continuity-recall-policy)
          :items items
          :disclosure (list :covered covered
                            :represented []
                            :omitted omissions
                            :recallable recallable)
          :receipt (list :tape-head tape-head
                         :read-status 'ok
                         :selection-policy
                         superchat-view--continuity-recall-policy
                         :selection-policy-version 0)
          :recall (list :selector selector :reason reason))))

(defun superchat-view--continuity-blocked-recall-envelope
    (session-id selector reason through-storage-id tape-head omissions)
  "Return a blocked Recall envelope with exact OMISSIONS."
  (list :contract "context-continuity-v0"
        :status 'blocked
        :prompt ""
        :scope (list :session-id session-id
                     :purpose 'recall
                     :through-storage-id through-storage-id
                     :lineage (vector
                               (list :session-id session-id
                                     :through-storage-id through-storage-id))
                     :selection-policy
                     superchat-view--continuity-recall-policy)
        :items []
        :disclosure (list :covered [] :represented []
                          :omitted omissions :recallable [])
        :receipt (list :tape-head tape-head
                       :read-status 'ok
                       :selection-policy
                       superchat-view--continuity-recall-policy
                       :selection-policy-version 0)
        :recall (list :selector selector :reason reason)))

(defun superchat-view--continuity-blocked-anchor-envelope
    (session-id selector reason anchor-id tape-head omission-reason)
  "Return a blocked Anchor recall envelope for OMISSION-REASON."
  (superchat-view--continuity-blocked-recall-envelope
   session-id selector reason anchor-id tape-head
   (vector
    (list :session-id session-id
          :range (vector anchor-id anchor-id)
          :reason omission-reason
          :recoverable 'unknown))))

(defun superchat-view--continuity-recall-anchor
    (session-id selector reason max-entries max-chars)
  "Validate and recall the Anchor named by SELECTOR from SESSION-ID."
  (let ((anchor-id (plist-get selector :anchor-id)))
    (unless (and (= (length selector) 2)
                 (integerp anchor-id) (> anchor-id 0))
      (error "Recall Anchor selector must contain one positive ID"))
    (let* ((all-rows (superchat-view-entries session-id))
           (tape-head (and all-rows (nth 0 (car (last all-rows)))))
           (anchor-row
            (cl-find-if (lambda (row)
                          (and (= (nth 0 row) anchor-id)
                               (string= (nth 2 row) "anchor")))
                        all-rows))
           (source-ids
            (when anchor-row
              (let* ((json-object-type 'alist)
                     (json-array-type 'vector)
                     (json-key-type 'symbol)
                     (meta (condition-case nil
                               (json-read-from-string (or (nth 4 anchor-row)
                                                         "{}"))
                             (error nil)))
                     (payload (or (superchat-view--object-get meta '(payload))
                                  meta)))
                (superchat-view--object-get payload '(source_ids)))))
           (valid-source-ids
            (and (vectorp source-ids)
                 (cl-every (lambda (id)
                             (and (integerp id) (> id 0)))
                           (append source-ids nil)))))
      (if valid-source-ids
          (let* ((ids (append source-ids nil))
                 (source-rows
                  (cl-remove-if-not
                   (lambda (row) (member (nth 0 row) ids))
                   all-rows))
                 (source-records
                  (mapcar (lambda (row)
                            (superchat-view--continuity-record session-id row))
                          source-rows)))
            (if (and (= (length source-rows) (length ids))
                     (cl-every #'superchat-view--continuity-material-record-p
                               source-records))
                (superchat-view--continuity-recall-envelope
                 session-id selector reason anchor-id tape-head source-records
                 max-entries max-chars)
              (superchat-view--continuity-blocked-anchor-envelope
               session-id selector reason anchor-id tape-head
               'missing-source)))
        (superchat-view--continuity-blocked-anchor-envelope
         session-id selector reason anchor-id tape-head
         'malformed-provenance)))))

(defun superchat-view--continuity-recall-storage-ids
    (session-id selector reason max-entries max-chars)
  "Recall the explicit storage IDs in SELECTOR from SESSION-ID."
  (let ((ids (plist-get selector :storage-ids)))
    (unless (and (= (length selector) 2)
                 (vectorp ids) (> (length ids) 0)
                 (cl-every (lambda (id) (and (integerp id) (> id 0)))
                           (append ids nil)))
      (error "Recall storage selector must contain positive IDs"))
    (let* ((id-list (delete-dups (append ids nil)))
           (all-rows (superchat-view-entries session-id))
           (tape-head (and all-rows (nth 0 (car (last all-rows)))))
           (source-rows
            (cl-remove-if-not (lambda (row) (member (nth 0 row) id-list))
                              all-rows))
           (missing-ids
            (cl-remove-if
             (lambda (id)
               (cl-find id source-rows :key (lambda (row) (nth 0 row))))
             id-list))
           (records
            (mapcar (lambda (row)
                      (superchat-view--continuity-record session-id row))
                    source-rows)))
      (if (null missing-ids)
          (superchat-view--continuity-recall-envelope
           session-id selector reason (apply #'max id-list) tape-head records
           max-entries max-chars)
        (superchat-view--continuity-blocked-recall-envelope
         session-id selector reason (apply #'max id-list) tape-head
         (superchat-view--continuity-id-omission-ranges
          session-id missing-ids 'missing-source 'no))))))

(defun superchat-view--continuity-recall-range
    (session-id selector reason max-entries max-chars)
  "Recall the validated range SELECTOR from SESSION-ID."
  (let* ((bounds (plist-get selector :range))
         (lo (aref bounds 0))
         (hi (aref bounds 1))
         (all-rows (superchat-view-entries session-id))
         (tape-head (and all-rows (nth 0 (car (last all-rows)))))
         (range-rows
          (cl-remove-if-not
           (lambda (row)
             (let ((id (nth 0 row)))
               (and (>= id lo) (<= id hi))))
           all-rows))
         (candidate-records
          (mapcar (lambda (row)
                    (superchat-view--continuity-record session-id row))
                  range-rows))
         (selection
          (superchat-view--continuity-select-records
           candidate-records (or max-entries 50) (or max-chars 4000)
           #'superchat-view--continuity-recall-item-line))
         (records (plist-get selection :records))
         (boundary-record (plist-get selection :boundary-record))
         (boundary-reason (plist-get selection :boundary-reason))
         (selected-ids
          (mapcar #'superchat-view--continuity-record-id records))
         (classified-omissions
          (delq
           nil
           (mapcar
            (lambda (record)
              (cond
               ((member (superchat-view--continuity-record-id record)
                        selected-ids)
                nil)
               ((and (eq boundary-reason 'oversized)
                     (= (superchat-view--continuity-record-id record)
                        (superchat-view--continuity-record-id
                         boundary-record)))
                (cons record 'oversized))
               (t (cons record 'budget))))
            candidate-records)))
         (items (vconcat
                 (mapcar #'superchat-view--continuity-raw-item records)))
         (covered (superchat-view--continuity-record-ranges records))
         (budget-omissions
          (superchat-view--continuity-omission-ranges classified-omissions))
         (missing-omissions
          (superchat-view--continuity-missing-ranges
           session-id lo hi
           (mapcar #'superchat-view--continuity-record-id candidate-records)))
         (omissions (superchat-view--continuity-merge-ranges
                     budget-omissions missing-omissions))
         (recallable (superchat-view--continuity-recallable-ranges
                      classified-omissions))
         (omitted-count
          (+ (length classified-omissions)
             (superchat-view--continuity-range-row-count missing-omissions))))
    (list :contract "context-continuity-v0"
          :status (if (> omitted-count 0) 'partial 'satisfied)
          :prompt (superchat-view--continuity-prompt
                   items covered omitted-count (> (length recallable) 0)
                   #'superchat-view--continuity-recall-item-line)
          :scope (list :session-id session-id
                       :purpose 'recall
                       :through-storage-id hi
                       :lineage (vector
                                 (list :session-id session-id
                                       :through-storage-id hi))
                       :selection-policy
                       superchat-view--continuity-recall-policy)
          :items items
          :disclosure (list :covered covered
                            :represented []
                            :omitted omissions
                            :recallable recallable)
          :receipt (list :tape-head tape-head
                         :read-status 'ok
                         :selection-policy
                         superchat-view--continuity-recall-policy
                         :selection-policy-version 0)
          :recall (list :selector selector :reason reason))))

(cl-defun superchat-view-recall
    (session-id selector &key reason max-entries max-chars)
  "Recall an explicit bounded SELECTOR from SESSION-ID without writing Tape."
  (unless (and (stringp session-id) (> (length session-id) 0))
    (error "SESSION-ID must be a non-empty string"))
  (unless (and (stringp reason) (> (length reason) 0))
    (error "Recall reason must be a non-empty string"))
  (dolist (budget (list max-entries max-chars))
    (unless (or (null budget) (natnump budget))
      (error "Recall budgets must be non-negative integers")))
  (unless (and (proper-list-p selector) (= (length selector) 2))
    (error "Recall selector must contain exactly one selector key"))
  (let ((selector-kind
         (cond
          ((plist-member selector :anchor-id)
           (let ((anchor-id (plist-get selector :anchor-id)))
             (unless (and (integerp anchor-id) (> anchor-id 0))
               (error "Recall Anchor selector must contain one positive ID")))
           'anchor)
          ((plist-member selector :storage-ids)
           (let ((ids (plist-get selector :storage-ids)))
             (unless (and (vectorp ids) (> (length ids) 0)
                          (cl-every
                           (lambda (id) (and (integerp id) (> id 0)))
                           (append ids nil)))
               (error "Recall storage selector must contain positive IDs")))
           'storage-ids)
          ((plist-member selector :range)
           (let* ((bounds (plist-get selector :range))
                  (lo (and (vectorp bounds) (= (length bounds) 2)
                           (aref bounds 0)))
                  (hi (and lo (aref bounds 1))))
             (unless (and (integerp lo) (> lo 0)
                          (integerp hi) (>= hi lo))
               (error "Recall selector must be one positive ordered :range")))
           'range)
          (t (error "Unsupported Recall selector")))))
    (condition-case nil
        (pcase selector-kind
          ('anchor
           (superchat-view--continuity-recall-anchor
            session-id selector reason max-entries max-chars))
          ('storage-ids
           (superchat-view--continuity-recall-storage-ids
            session-id selector reason max-entries max-chars))
          ('range
           (superchat-view--continuity-recall-range
            session-id selector reason max-entries max-chars)))
      (error
       (superchat-view--continuity-unknown-envelope
        session-id 'recall superchat-view--continuity-recall-policy
        (list :selector selector :reason reason))))))

;; ═══════════════════════════════════════════════════════════
;; Formatting for LLM consumption
;; ═══════════════════════════════════════════════════════════

(defun superchat-view--format-row (row)
  "Format a single tape ROW as a string for inclusion in prompts.
ROW layout: (id session_id topic kind content meta created_at)."
  (let ((kind (nth 3 row))
        (content (nth 4 row))
        (created (nth 6 row)))
    (format "[%s @ %s]\n%s" kind created content)))

(defun superchat-view-format-entries (rows)
  "Format ROWS as a single string suitable for a prompt context block."
  (if (null rows)
      ""
    (mapconcat #'superchat-view--format-row rows "\n\n")))

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

(provide 'superchat-tape-view)

;;; superchat-tape-view.el ends here
