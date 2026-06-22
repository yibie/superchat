;;; superchat-tape-view.el — Tape views for Superchat -*- lexical-binding: t; -*-

;;; Commentary:

;; View functions over the SQLite tape table.  Views are runtime
;; collections of entries; they do not introduce new storage structures.
;; This follows the tape.systems primitive model:
;;   Entry (immutable) → Anchor (marked distillation) → View (collection).

;;; Code:

(require 'cl-lib)

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

(defun superchat-view-anchor-source-ids (anchor-row)
  "Extract source entry ids from an ANCHOR-ROW's meta JSON."
  (when anchor-row
    (let ((meta (condition-case nil
                    (json-read-from-string (or (nth 3 anchor-row) "{}"))
                  (error (make-hash-table :test 'equal)))))
      (if (hash-table-p meta)
          (gethash "source_ids" meta nil)
        (cdr (assoc 'source_ids meta))))))

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
