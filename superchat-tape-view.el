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
