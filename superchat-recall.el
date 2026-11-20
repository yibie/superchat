;;; superchat-recall.el --- The tools the model uses on the Tape -*- lexical-binding: t; -*-

;;; Commentary:
;; recall, tape_query and note.  Nothing here keeps state of its own.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-tools)
(require 'superchat-view)

(defconst superchat-recall--max-chars 6000
  "Longest text `recall' returns.")

(defconst superchat-recall--max-rows 200
  "Most rows `tape_query' returns.")

(defun superchat-recall--scope ()
  "The scope of the current timeline."
  (plist-get (car (superchat-tape-entries :tape superchat-tool-tape :limit 1 :newest t))
             :scope))

(defun superchat-recall--view (args)
  "Run the view named in ARGS for the current timeline and scope; text."
  (let ((view (intern (plist-get args :view)))
        (params (list :scope (superchat-recall--scope) :tape superchat-tool-tape)))
    (dolist (key '(:query :name :id :limit))
      (when-let* ((value (plist-get args key)))
        (setq params (plist-put params key (if (memq key '(:id :limit))
                                               (superchat-tools--number value nil)
                                             value)))))
    (superchat-view-render (apply #'superchat-view view params)
                           superchat-recall--max-chars)))

(defun superchat-recall--query (args)
  "Rows of the read-only SQL in ARGS as JSON lines.
The limit is part of the statement, so a runaway query stops at the database."
  (let* ((sql (string-trim-right (plist-get args :sql) "[ \t\n;]+"))
         (rows (superchat-tape-select
                (format "SELECT * FROM (%s\n) LIMIT %d" sql (1+ superchat-recall--max-rows))))
         (lines (mapcar (lambda (row) (json-serialize (vconcat row) :null-object nil))
                        (take superchat-recall--max-rows rows))))
    (string-join
     (append lines
             (and (nthcdr superchat-recall--max-rows rows) (list "[more rows left out]"))
             (and (null rows) (list "[no rows]")))
     "\n")))

(defun superchat-recall-note (tape title text &optional refs scope)
  "Append a note TITLE, TEXT citing the entry ids REFS to timeline TAPE.
SCOPE is needed only when TAPE has no entry yet.  The model and the person
write notes through this one function."
  (superchat-tape-append tape 'note text :data (list :title title) :refs refs :scope scope))

(defun superchat-recall--note (args)
  "Append the note described by ARGS to the current timeline; one line."
  (let* ((ids (mapcar #'string-to-number
                      (split-string (or (plist-get args :sources) "") "[, \t]+" t)))
         (bad (cl-remove-if #'superchat-tape-get ids)))
    (when bad
      (error "Not entries: %s" (mapconcat #'number-to-string bad ", ")))
    (format "Note saved as #%d."
            (plist-get (superchat-recall-note superchat-tool-tape (plist-get args :title)
                                              (plist-get args :text) ids)
                       :id))))

(superchat-tool-define
 "recall" :class 'read :function #'superchat-recall--view :deferred t
 :description
 (lambda ()
   (concat "Recall from the Tape, which holds everything that happened in every conversation. "
           "Give the name of a view and its arguments; at most 6000 characters come back. Views:\n"
           (mapconcat (lambda (v) (format "%s: %s" (car v) (cdr v)))
                      (superchat-view-names) "\n")))
 :args '((:name "view" :type string :description "Name of the view" :required t)
         (:name "query" :type string :description "Text to search for")
         (:name "name" :type string :description "Topic name")
         (:name "id" :type number :description "Entry id")
         (:name "limit" :type number :description "Most entries")))

(superchat-tool-define
 "tape_query" :class 'read :function #'superchat-recall--query :deferred t
 :description
 (concat "Run a read-only SELECT on the Tape; rows come back as JSON arrays, one per line, "
         "at most 200. Table: entry(id INTEGER PRIMARY KEY, tape TEXT, kind TEXT, "
         "text TEXT, data TEXT (JSON), refs TEXT (JSON array of ids), scope TEXT, at REAL). "
         "kind is user, assistant, tool, anchor, note or event; entry_fts indexes text.")
 :args '((:name "sql" :type string :description "A SELECT statement" :required t)))

(superchat-tool-define
 "note" :class 'read :function #'superchat-recall--note :deferred t
 :description "Keep a conclusion worth remembering, with the entries it comes from. A note among its sources is replaced by this one: cite a note only to correct it or merge it."
 :args '((:name "title" :type string :description "Short title" :required t)
         (:name "text" :type string :description "The conclusion" :required t)
         (:name "sources" :type string :description "Comma-separated entry ids" :required t)))

(provide 'superchat-recall)
;;; superchat-recall.el ends here
