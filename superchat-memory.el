;;; superchat-memory.el --- SQLite-backed memory facade for Superchat -*- lexical-binding: t; -*-

;; Author: Superchat contributors
;; Keywords: ai, chat, memory

;;; Commentary:
;; This module is a thin compatibility facade over `superchat-db'.
;;
;; Storage moved from org-mode (`memory.org', `soul.org') to SQLite in
;; v0.8.  The org-only complexity — org-ql search, RELATED multi-hop
;; BFS, keyword LLM enrichment, contradiction pairing, mood taxonomy,
;; ACCESS_COUNT decay, soul synthesis — was deleted.  External
;; consumers see the same function names and (compatible) call sites.
;;
;; Public API preserved:
;;   superchat-memory-compose-title
;;   superchat-memory-capture-explicit
;;   superchat-memory-capture-conversation
;;   superchat-memory-auto-capture
;;   superchat-memory-retrieve
;;   superchat-memory-summarize-session-history
;;   superchat-memory-prune
;;   superchat-memory-add-raw         (kept as a thin tape-append shim)
;;   superchat-memory-import-from-org (one-shot migration helper)
;;
;; Anything else from v0.7 is gone.  If you depended on `-retrieve-async',
;; `-retrieve-with-context', `-discover-keyword-associations',
;; `-expand-with-related', `-synthesize-soul', `-llm-keyword-enricher',
;; the mood taxonomy, or `superchat-memory-soul-file', update your code
;; to the surviving API.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org nil t)             ; org-with-wide-buffer etc (optional)
(require 'superchat-db)

(defvar superchat-data-directory)        ; from superchat.el
(defvar superchat-llm-backend)           ; from superchat.el
(declare-function superchat-db-open                 "superchat-db" ())
(declare-function superchat-db-memory-insert        "superchat-db" (content &rest args))
(declare-function superchat-db-memory-search        "superchat-db" (query &optional limit))
(declare-function superchat-db-memory-search-simple "superchat-db" (query &optional limit))
(declare-function superchat-db-memory-count         "superchat-db" (&optional review-status))
(declare-function superchat-db-memory-delete-rejected-older-than "superchat-db" (days))
(declare-function superchat-db-tape-append          "superchat-db" (session-id kind content &rest args))

(defgroup superchat-memory nil
  "SQLite-backed long-term memory for Superchat."
  :group 'superchat)

(defcustom superchat-memory-auto-capture-enabled t
  "When non-nil, `superchat-memory-auto-capture' attempts implicit captures.

The current implementation is conservative: implicit memory writes that
used to call the LLM on the response hot path have been removed.
Implicit capture now only runs when the input matches one of
`superchat-memory-explicit-trigger-patterns'.  Anything else must be
captured by the user via `/remember' or by registering a custom
capture hook."
  :type 'boolean
  :group 'superchat-memory)

(defcustom superchat-memory-auto-capture-minimum-length 40
  "Minimum trimmed character count before auto-capture considers a turn."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-explicit-trigger-patterns
  '("\\`记住"
    "\\`记下"
    "\\`remember\\b"
    "\\`note this"
    "请记住"
    "请记下")
  "Regexps that mark a user turn as an explicit memory request.
Matched case-insensitively against the trimmed user input.  When any
pattern matches, `superchat-memory-auto-capture' stores the turn."
  :type '(repeat regexp)
  :group 'superchat-memory)

(defcustom superchat-memory-title-max-length 78
  "Maximum length for synthesized titles."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-prune-rejected-days 30
  "Delete rejected memory rows older than this many days during prune.
Set to 0 or negative to disable the rejected-row sweep."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-retrieve-limit 5
  "Default upper bound on rows returned by `superchat-memory-retrieve'."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-auto-recall-min-length 8
  "Minimum trimmed input length before auto-recall is attempted.
Below this width, the user message is treated as too terse to benefit
from memory retrieval (e.g. \"ok\", \"thanks\")."
  :type 'integer
  :group 'superchat-memory)


;;;; ── Helpers ────────────────────────────────────────────────────────

(defun superchat-memory--sanitize (s)
  "Trim S and collapse internal whitespace."
  (when (and s (stringp s))
    (let ((trimmed (string-trim s)))
      (replace-regexp-in-string "[ \t\n\r]+" " " trimmed))))

(defun superchat-memory--truncate (s n)
  "Truncate string S to at most N chars, with ellipsis."
  (if (and s (> (length s) n))
      (concat (substring s 0 (max 1 (- n 1))) "…")
    s))

(defun superchat-memory--row-to-plist (row)
  "Normalize a SQLite row to a plist matching the v0.7 retrieval shape.
ROW layout (from `superchat-db-memory-search' / `-search-simple'):
  (id title content keywords mood review_status created_at . _rest)."
  (when row
    (list :id         (nth 0 row)
          :title      (or (nth 1 row) "")
          :content    (or (nth 2 row) "")
          :keywords   (or (nth 3 row) "")
          :mood       (or (nth 4 row) "")
          :tags       ""
          :type       "memory"
          :timestamp  (or (nth 6 row) ""))))

(defun superchat-memory--extract-text (exchange)
  "Pick the most informative text from EXCHANGE.
EXCHANGE may be a string or a plist with :input / :user / :content."
  (cond
   ((stringp exchange) exchange)
   ((and (consp exchange) (plist-member exchange :input))
    (or (plist-get exchange :input) ""))
   ((and (consp exchange) (plist-member exchange :user))
    (or (plist-get exchange :user) ""))
   ((and (consp exchange) (plist-member exchange :content))
    (or (plist-get exchange :content) ""))
   (t (format "%S" exchange))))


;;;; ── Public API: title / write paths ───────────────────────────────

(defun superchat-memory-compose-title (content)
  "Compose a one-line title from CONTENT.
Returns a string bounded by `superchat-memory-title-max-length'."
  (let ((normalized (superchat-memory--sanitize content)))
    (or (and normalized
             (superchat-memory--truncate
              normalized superchat-memory-title-max-length))
        "Untitled")))

(cl-defun superchat-memory-capture-explicit (content &optional title
                                                     &key tags type keywords related)
  "Store CONTENT as an accepted memory.  Returns the new row id.
TITLE defaults to `superchat-memory-compose-title'.
TAGS, TYPE, RELATED are accepted for back-compat but only TAGS / TYPE
land in the keywords column.  KEYWORDS is honoured directly."
  (ignore related)
  (let* ((clean (or (superchat-memory--sanitize content) ""))
         (title (or title (superchat-memory-compose-title clean)))
         (kw (cond
              ((stringp keywords) keywords)
              ((listp keywords)   (mapconcat #'identity keywords ","))
              (t                  "")))
         (tag-str (cond
                   ((stringp tags) tags)
                   ((listp tags)   (mapconcat #'identity tags ","))
                   (t              "")))
         (combined (if (and (not (string-empty-p kw))
                            (not (string-empty-p tag-str)))
                       (concat kw "," tag-str)
                     (concat kw tag-str))))
    (when (> (length clean) 0)
      (superchat-db-memory-insert
       clean
       :title    title
       :keywords combined
       :mood     (or type "")
       :status   "accepted"))))

(cl-defun superchat-memory-capture-conversation (exchange &key tier tags type
                                                          keywords related
                                                          access-count)
  "Store EXCHANGE as an accepted memory.
TIER is accepted for back-compat (was used by the v0.6 dual-track flow)
but no longer changes behaviour; both Tier 1 and Tier 3 land in the
same memory table.  TAGS / TYPE / KEYWORDS / RELATED forwarded to
`superchat-memory-capture-explicit'.  ACCESS-COUNT is ignored — the
decay model that depended on it was removed."
  (ignore tier access-count)
  (let ((content (superchat-memory--extract-text exchange)))
    (when (and content (> (length (string-trim content)) 0))
      (superchat-memory-capture-explicit
       content nil
       :tags tags :type type :keywords keywords :related related))))

(defun superchat-memory-auto-capture (exchange)
  "Optionally capture EXCHANGE based on explicit-trigger patterns.
Honors `superchat-memory-auto-capture-enabled' and the length floor.
Returns the new row id on capture, nil otherwise."
  (when superchat-memory-auto-capture-enabled
    (let* ((text (superchat-memory--extract-text exchange))
           (trimmed (and text (string-trim text))))
      (when (and trimmed
                 (>= (length trimmed)
                     superchat-memory-auto-capture-minimum-length)
                 (let ((case-fold-search t))
                   (cl-some (lambda (rx) (string-match-p rx trimmed))
                            superchat-memory-explicit-trigger-patterns)))
        (superchat-memory-capture-explicit trimmed)))))

(cl-defun superchat-memory-add-raw (content &key mood context verbatim tags type)
  "Compatibility shim — append raw event text to the SQLite tape.
The v0.6 `soul.org' raw-event store is gone.  Callers that used to
write \"raw\" events now feed them into the conversation tape via
`superchat-db-tape-append', preserving append-only semantics without
the org overhead.  Returns the tape row id, or nil if the tape API is
unavailable."
  (ignore verbatim type)
  (when (and (fboundp 'superchat-db-tape-append)
             content
             (> (length (string-trim content)) 0))
    (let ((sid (format-time-string "raw-%Y%m%d"))
          (meta (list :mood    (or mood "")
                      :context (or context "")
                      :tags    (cond
                                ((stringp tags) tags)
                                ((listp tags)   (mapconcat #'identity tags ","))
                                (t              "")))))
      (ignore-errors
        (superchat-db-tape-append sid "system" content :meta meta)))))


;;;; ── Public API: read paths ────────────────────────────────────────

(defun superchat-memory-retrieve (query-string)
  "Return accepted memories matching QUERY-STRING as a list of plists.
Uses FTS5 first; falls back to LIKE when FTS5 returns nothing
(common for very short queries or non-ASCII tokens)."
  (when (and (stringp query-string)
             (not (string-empty-p (string-trim query-string))))
    (let* ((limit superchat-memory-retrieve-limit)
           (rows (or (ignore-errors
                       (superchat-db-memory-search query-string limit))
                     (superchat-db-memory-search-simple query-string limit))))
      (delq nil (mapcar #'superchat-memory--row-to-plist rows)))))

;;;; ── Maintenance ───────────────────────────────────────────────────

(defun superchat-memory-prune (&optional silent)
  "Delete rejected memory rows older than the configured horizon.
Returns nil.  Honours `superchat-memory-prune-rejected-days'."
  (interactive)
  (let ((days superchat-memory-prune-rejected-days))
    (when (and (integerp days) (> days 0))
      (ignore-errors
        (superchat-db-memory-delete-rejected-older-than days)))
    (unless silent
      (message "superchat-memory: pruned rejected rows older than %d days." days))))

(defun superchat-memory-summarize-session-history (history-content)
  "Compatibility shim — stash HISTORY-CONTENT as a pending memory row.

In v0.7 this returned an llm.el request that summarized the session on
restart.  That LLM call was on the startup hot path and blocked the
chat buffer.  v0.8 simply persists the raw history as a pending-review
memory so the user can decide later whether to keep it.

Returns a truthy value (the new row id) on success so existing callers
that check the return as a boolean continue to work."
  (when (and history-content
             (stringp history-content)
             (> (length (string-trim history-content)) 0))
    (ignore-errors
      (superchat-db-memory-insert
       history-content
       :title (superchat-memory-compose-title
               (concat "Session "
                       (format-time-string "%Y-%m-%d %H:%M")))
       :status "pending"))))


;;;; ── One-shot org → sqlite import ──────────────────────────────────

(defun superchat-memory-import-from-org (&optional org-file)
  "Import legacy `memory.org' entries into the SQLite memory table.
ORG-FILE defaults to memory.org under `superchat-data-directory'.
Each top-level heading becomes one accepted memory row with its
title, body, and any KEYWORDS / TAGS property preserved as keywords.
Idempotent only at the file level — re-running on the same file
will create duplicates.  Returns the number of rows inserted."
  (interactive)
  (let* ((file (or org-file
                   (expand-file-name "memory.org" superchat-data-directory)))
         (count 0))
    (unless (file-exists-p file)
      (user-error "No file at %s" file))
    (require 'org)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\* " nil t)
         (let* ((title (org-get-heading t t t t))
                (props (org-entry-properties))
                (content (save-excursion
                           (org-back-to-heading t)
                           (org-end-of-meta-data t)
                           (buffer-substring-no-properties (point)
                                                           (or (outline-next-heading)
                                                               (point-max)))))
                (keywords (or (cdr (assoc "KEYWORDS" props)) ""))
                (tags     (or (cdr (assoc "TAGS" props))     "")))
           (when (and content (> (length (string-trim content)) 0))
             (superchat-db-memory-insert
              (string-trim content)
              :title    title
              :keywords (if (string-empty-p tags) keywords
                          (concat keywords "," tags))
              :status   "accepted")
             (cl-incf count))))))
    (when (called-interactively-p 'interactive)
      (message "superchat-memory: imported %d row(s) from %s" count file))
    count))


(provide 'superchat-memory)

;;; superchat-memory.el ends here
