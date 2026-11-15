;;; superchat-view.el --- Named functions from arguments to entries -*- lexical-binding: t; -*-

;;; Commentary:
;; Context for the model is always built from a view; nothing is carried over
;; implicitly.  Views only read the Tape.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat)
(require 'superchat-tape)

(defvar superchat-view--registry nil
  "Alist of (NAME FUNCTION . DOC), in definition order.")

(defun superchat-view-define (name function doc)
  "Define view NAME, a symbol.  FUNCTION takes a plist and returns entries."
  (setq superchat-view--registry
        (append (assq-delete-all name superchat-view--registry)
                (list (cons name (cons function doc))))))

(defun superchat-view-remove (name)
  "Remove view NAME."
  (setq superchat-view--registry (assq-delete-all name superchat-view--registry)))

(defun superchat-view (name &rest args)
  "The entries of view NAME for the plist ARGS."
  (funcall (or (cadr (assq name superchat-view--registry))
               (error "Unknown view: %s" name))
           args))

(defun superchat-view-names ()
  "Alist of (NAME . DOC) of every view."
  (mapcar (lambda (v) (cons (car v) (cddr v))) superchat-view--registry))

(defun superchat-view--clip (text max)
  "TEXT cut to at most MAX characters, ending in a mark that says how many
were cut; TEXT itself when it fits."
  (if (<= (length text) max)
      text
    (let ((keep (max 0 (- max (length (format "... [%d characters cut]" (length text)))))))
      (format "%s... [%d characters cut]" (substring text 0 keep) (- (length text) keep)))))

(defun superchat-view--cut (entry)
  "The text of ENTRY, a tool result cut to 400 characters."
  (let ((text (plist-get entry :text)))
    (if (and (eq (plist-get entry :kind) 'tool) (> (length text) 400))
        (concat (substring text 0 400) "...")
      text)))

(defun superchat-view--omitted (n)
  "The line that says N entries were dropped, or nothing."
  (if (> n 0) (format "[%d earlier entries left out]\n\n" n) ""))

(defun superchat-view-render (entries &optional max-chars)
  "ENTRIES as text.  Over MAX-CHARS the oldest are dropped and counted; a
single block still over it has its text cut, with a mark saying so.  The
header line of a block is never cut: it is what the text is cited by."
  (let* ((blocks (mapcar (lambda (e) (cons (format "#%d %s %s" (plist-get e :id) (plist-get e :kind)
                                                   (format-time-string "%F %R" (plist-get e :at)))
                                           (superchat-view--cut e)))
                         entries))
         (size (lambda (b) (+ (length (car b)) 1 (length (cdr b)))))
         (total (cl-loop for b in blocks sum (+ 2 (funcall size b)) into n finally return (- n 2)))
         (dropped 0))
    (while (and max-chars (cdr blocks)
                (> (+ total (length (superchat-view--omitted dropped))) max-chars))
      (cl-decf total (+ 2 (funcall size (pop blocks))))
      (cl-incf dropped))
    (when max-chars
      (let ((room (- max-chars (length (superchat-view--omitted dropped)))))
        (setq blocks (mapcar (lambda (b)
                               (cons (car b) (superchat-view--clip
                                              (cdr b) (max 0 (- room (length (car b)) 1)))))
                             blocks))))
    (concat (superchat-view--omitted dropped)
            (mapconcat (lambda (b) (concat (car b) "\n" (cdr b))) blocks "\n\n"))))

(defun superchat-view--in-order (entries)
  "ENTRIES without duplicates, in id order."
  (sort (cl-remove-duplicates entries :key (lambda (e) (plist-get e :id)))
        (lambda (a b) (< (plist-get a :id) (plist-get b :id)))))

(defun superchat-view--conversation (args)
  "The timeline of :tape from its latest anchor; see the view `conversation'."
  (let* ((tape (plist-get args :tape))
         (anchor (car (superchat-tape-entries :tape tape :kinds '(anchor)
                                              :newest t :limit 1)))
         (rows (superchat-tape-entries
                :tape tape :after (and anchor (1- (plist-get anchor :id)))))
         (first (car (superchat-tape-entries :tape tape :kinds '(anchor) :limit 1))))
    ;; A fork's first anchor has no summary: the entries it refers to are the past.
    (if (and anchor (eq (plist-get anchor :id) (plist-get first :id))
             (plist-get anchor :refs) (string-empty-p (plist-get anchor :text)))
        (append (delq nil (mapcar #'superchat-tape-get
                                  (sort (copy-sequence (plist-get anchor :refs)) #'<)))
                rows)
      rows)))

(defun superchat-view--topic (args)
  "The entries from each anchor named :name to the next anchor of its timeline."
  (let ((scope (plist-get args :scope)))
    (cl-loop
     for (id next) in
     (superchat-tape-select
      (concat "SELECT a.id, (SELECT MIN(b.id) FROM entry b WHERE b.tape = a.tape"
              " AND b.kind = 'anchor' AND b.id > a.id) FROM entry a"
              " WHERE a.kind = 'anchor' AND json_extract(a.data, '$.name') = ?"
              (if scope " AND a.scope = ?" "") " ORDER BY a.id")
      (append (list (plist-get args :name)) (and scope (list scope))))
     append (superchat-tape-entries :tape (plist-get (superchat-tape-get id) :tape)
                                    :after (1- id) :before next))))

(defun superchat-view--search (args)
  "Hits of :query with :around neighbours each side, in id order."
  (let ((around (or (plist-get args :around) 1)))
    (superchat-view--in-order
     (cl-loop
      for hit in (superchat-tape-search (plist-get args :query) :scope (plist-get args :scope)
                                        :kinds (plist-get args :kinds)
                                        :limit (or (plist-get args :limit) 8))
      for id = (plist-get hit :id)
      for tape = (plist-get hit :tape)
      collect hit
      when (> around 0)
      append (superchat-tape-entries :tape tape :before id :limit around :newest t)
      and append (superchat-tape-entries :tape tape :after id :limit around)))))

(defun superchat-view--notes (args)
  "Notes, newest first; with :scope, that scope and \"home\"."
  (let* ((scope (plist-get args :scope))
         (query (plist-get args :query))
         (limit (or (plist-get args :limit) 20))
         (found (cl-loop
                 for s in (if scope (delete-dups (list scope "home")) '(nil))
                 append (if query
                            (superchat-tape-search query :scope s :kinds '(note) :limit limit)
                          (superchat-tape-entries :scope s :kinds '(note)
                                                  :newest t :limit limit)))))
    (take limit (nreverse (superchat-view--in-order found)))))

(defun superchat-view--timelines (args)
  "Per timeline, newest first: its latest anchor, else its first user entry."
  (let ((scope (plist-get args :scope)))
    (mapcar (lambda (row) (superchat-tape-get (car row)))
            (superchat-tape-select
             (concat "SELECT COALESCE("
                     "(SELECT MAX(id) FROM entry WHERE tape = e.tape AND kind = 'anchor'),"
                     "(SELECT MIN(id) FROM entry WHERE tape = e.tape AND kind = 'user'))"
                     " FROM entry e WHERE tape IN (SELECT tape FROM entry"
                     " WHERE kind IN ('anchor', 'user')) AND tape NOT IN "
                     "(SELECT tape FROM entry WHERE kind = 'user'"
                     " AND json_extract(data, '$.ephemeral') = 1)"
                     (if scope " AND scope = ?" "")
                     " GROUP BY tape ORDER BY MAX(id) DESC LIMIT ?")
             (append (and scope (list scope))
                     (list (or (plist-get args :limit) 30)))))))

(defun superchat-view--sources (args)
  "The entries named by the refs of entry :id."
  (when-let* ((entry (superchat-tape-get (plist-get args :id))))
    (superchat-view--in-order
     (delq nil (mapcar #'superchat-tape-get (plist-get entry :refs))))))

(superchat-view-define
 'conversation #'superchat-view--conversation
 ":tape. The timeline from its latest anchor to its end; all of it with no anchor.")
(superchat-view-define
 'topic #'superchat-view--topic
 ":name, :scope optional. Entries from each anchor of that name to the next anchor.")
(superchat-view-define
 'search #'superchat-view--search
 ":query, :scope, :kinds, :limit (8), :around (1). Hits with neighbours, in id order.")
(superchat-view-define
 'notes #'superchat-view--notes
 ":scope optional, :query optional, :limit (20). Notes, newest first.")
(superchat-view-define
 'timelines #'superchat-view--timelines
 ":scope optional, :limit (30). Per timeline, latest anchor else first user entry.")
(superchat-view-define
 'sources #'superchat-view--sources
 ":id. The entries that entry ID refers to.")

(provide 'superchat-view)
;;; superchat-view.el ends here
