;;; test-tape-view.el --- Tests for tape views -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the tape view layer and tape FTS5.

;;; Code:

(require 'ert)
(require 'superchat-db)
(require 'superchat-tape-view)

(defvar superchat-data-directory)

(defun test-tape-view--with-temp-db (body)
  "Run BODY with a fresh temporary database."
  (let* ((tmp-dir (make-temp-file "superchat-tape-view-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil))
    (unwind-protect
        (progn
          (superchat-db-open db-file)
          (funcall body))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest test-tape-append-accepts-topic ()
  "`superchat-db-tape-append' should accept a topic argument."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((id (superchat-db-tape-append "s1" "user" "hello" :topic "intro")))
       (should (integerp id))
       (let ((rows (superchat-db-tape-select
                    "SELECT topic, kind, content FROM tape WHERE id = ?"
                    (list id))))
         (should (= (length rows) 1))
         (should (string= "intro" (caar rows)))
         (should (string= "user" (cadar rows)))
         (should (string= "hello" (caddar rows))))))))

(ert-deftest test-tape-fts5-search ()
  "FTS5 search should find entries by content."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "incremental indexing with FTS5")
     (superchat-db-tape-append "s2" "user" "something unrelated")
     (let ((rows (superchat-db-tape-search "FTS5" nil 10)))
       (should (= (length rows) 1))
       (should (string-match-p "FTS5" (nth 4 (car rows))))))))

(ert-deftest test-tape-fts5-search-accepts-apostrophes ()
  "FTS5 tape searches must not turn an apostrophe into SQL syntax."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "user's note about FTS5")
     (let ((rows (superchat-db-tape-search "user's" nil 10)))
       (should (= (length rows) 1))
       (should (string-match-p "user's" (nth 4 (car rows))))))))

(ert-deftest test-tape-v2-to-v3-migration ()
  "Opening a schema-v2 database must migrate to v3 and create tape_fts."
  (let* ((tmp-dir (make-temp-file "superchat-tape-migration-test-" t))
         (db-file (expand-file-name "superchat.db" tmp-dir))
         (superchat-data-directory tmp-dir)
         (superchat-db--connection nil)
         (superchat-db--path nil)
         (db (sqlite-open db-file)))
    (unwind-protect
        (progn
          ;; Manufacture a v2 database: no topic column, no tape_fts.
          (sqlite-execute
           db
           "CREATE TABLE tape (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              session_id TEXT NOT NULL,
              kind TEXT NOT NULL,
              content TEXT NOT NULL,
              meta TEXT DEFAULT '{}',
              created_at TEXT NOT NULL DEFAULT (datetime('now')))")
          (sqlite-execute
           db
           "CREATE TABLE memory (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              title TEXT, content TEXT NOT NULL,
              keywords TEXT DEFAULT '', source_tape_ids TEXT DEFAULT '[]',
              mood TEXT DEFAULT '', replaced_by INTEGER REFERENCES memory(id),
              review_status TEXT DEFAULT 'pending',
              created_at TEXT NOT NULL DEFAULT (datetime('now')))")
          (sqlite-execute
           db
           "CREATE VIRTUAL TABLE memory_fts USING fts5(
              content, keywords, title,
              content=memory, content_rowid=id,
              tokenize='trigram')")
          (sqlite-execute
           db
           "CREATE TABLE _schema_version (version INTEGER PRIMARY KEY)")
          (sqlite-execute
           db
           "INSERT INTO _schema_version (version) VALUES (2)")
          (sqlite-execute
           db
           "INSERT INTO tape (session_id, kind, content) VALUES ('migration-test', 'user', 'pre-v3 content')")
          (sqlite-close db)
          ;; Now open with superchat-db and verify migration.
          (setq db (superchat-db-open db-file))
          (should (= 3 (superchat-db--current-schema-version db)))
          (should (cl-some (lambda (col) (string= "topic" (cadr col)))
                           (sqlite-select db "PRAGMA table_info(tape)")))
          (should (sqlite-select
                   db
                   "SELECT 1 FROM sqlite_master WHERE type='table' AND name='tape_fts'"))
          (let ((rows (superchat-db-tape-search "content" nil 10)))
            (should (= 1 (length rows)))
            (should (string-match-p "pre-v3 content" (nth 4 (car rows))))))
      (superchat-db-close)
      (delete-directory tmp-dir t))))

(ert-deftest test-tape-fts5-search-scoped-to-session ()
  "FTS5 search can be scoped to a session."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "hello world")
     (superchat-db-tape-append "s2" "user" "hello again")
     (let ((rows (superchat-view-search "hello" "s1" 10)))
       (should (= (length rows) 1))
       (should (string= "s1" (nth 1 (car rows))))))))

(ert-deftest test-tape-select-read-only-guard ()
  "`superchat-db-tape-select' should reject mutating statements."
  (test-tape-view--with-temp-db
   (lambda ()
     (should-error (superchat-db-tape-select "DELETE FROM tape"))
     (should-error (superchat-db-tape-select "INSERT INTO tape VALUES (1,2,3,4,5,6)"))
     (should-error (superchat-db-tape-select "UPDATE tape SET content = 'x'")))))

(ert-deftest test-view-file-history ()
  "`superchat-view-file-history' should find tool entries mentioning a path."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append
      "s1" "tool_call" "write-file"
      :meta '((name . "write-file") (args . ((path . "/tmp/init.el")))))
     (superchat-db-tape-append
      "s1" "tool_result" "wrote /tmp/init.el")
     (superchat-db-tape-append
      "s1" "tool_call" "read-file"
      :meta '((name . "read-file") (args . ((path . "/tmp/other.el")))))
     (let ((rows (superchat-view-file-history "/tmp/init.el" "s1" 10)))
       (should (= (length rows) 2))))))

(ert-deftest test-view-tool-history ()
  "`superchat-view-tool-history' should find entries for a specific tool."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append
      "s1" "tool_call" "shell-command"
      :meta '((name . "shell-command") (args . ((command . "ls")))))
     (superchat-db-tape-append
      "s1" "tool_result" "file1 file2")
     (superchat-db-tape-append
      "s1" "tool_call" "search-text"
      :meta '((name . "search-text") (args . ((pattern . "foo")))))
     (let ((rows (superchat-view-tool-history "shell-command" "s1" 10)))
       ;; tool_result does not carry the tool name in meta, so only
       ;; the tool_call row matches directly.
       (should (= (length rows) 1))))))

(ert-deftest test-view-recent-errors ()
  "`superchat-view-recent-errors' should find error-like tool results."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "tool_result" "Error: file not found")
     (superchat-db-tape-append "s1" "tool_result" "Success")
     (superchat-db-tape-append "s1" "tool_result" "Traceback: some error")
     (let ((rows (superchat-view-recent-errors "s1" 10)))
       (should (= (length rows) 2))))))

(ert-deftest test-view-expand-anchor ()
  "`superchat-view-expand-anchor' should return source entries."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((id1 (superchat-db-tape-append "s1" "user" "msg1"))
            (id2 (superchat-db-tape-append "s1" "assistant" "msg2"))
            (anchor-id (superchat-db-tape-append
                        "s1" "anchor" "summary"
                        :meta `((source_ids . [,id1 ,id2]))))
            (anchor (superchat-view-anchor-latest "s1"))
            (rows (superchat-view-expand-anchor "s1" anchor)))
       (should (= (length rows) 2))
       (should (string= "msg1" (nth 3 (car rows))))
       (should (string= "msg2" (nth 3 (cadr rows))))))))

;; ═══════════════════════════════════════════════════════════
;; Slash-command integration
;; ═══════════════════════════════════════════════════════════

(ert-deftest test-cmd-remember-writes-tape-anchor ()
  "`/remember' should write both a user entry and an anchor to tape."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "remember-test")
           (superchat--pending-recalled-memories nil)
           (legacy-calls nil))
       (cl-letf (((symbol-function 'superchat-memory-capture-explicit)
                  (lambda (content &optional title)
                    (push (list content title) legacy-calls))))
         (let ((result (superchat--cmd-remember
                        "remember" "always use lexical-binding" nil nil nil)))
           (should (string-match-p "Anchor added" (plist-get result :content)))
           ;; Tape should have a user entry and an anchor.
           (let ((rows (superchat-db-tape-select
                        "SELECT kind, content FROM tape WHERE session_id = ? ORDER BY id ASC"
                        (list "remember-test"))))
             (should (= (length rows) 2))
             (should (string= "user" (caar rows)))
             (should (string-match-p "always use lexical-binding" (cadar rows)))
             (should (string= "anchor" (caadr rows)))
             (should (string= "always use lexical-binding" (cadadr rows))))
           ;; Legacy memory should still be called for backward compat.
           (should (= (length legacy-calls) 1))))))))

(ert-deftest test-cmd-recall-searches-tape-first ()
  "`/recall' should set pending memories from tape search results."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "recall-test")
           (superchat--pending-recalled-memories nil))
       (superchat-db-tape-append "recall-test" "user" "incremental indexing")
       (superchat-db-tape-append "recall-test" "assistant" "use trigram tokenizer")
       (superchat--cmd-recall "recall" "trigram" nil nil nil)
       (should (> (length superchat--pending-recalled-memories) 0))
       (should (cl-some (lambda (m)
                          (string-match-p "trigram" (plist-get m :content)))
                        superchat--pending-recalled-memories))))))

;; ═══════════════════════════════════════════════════════════
;; Tape tool integration
;; ═══════════════════════════════════════════════════════════

(ert-deftest test-tool-sql-read-only ()
  "`superchat-tool-sql' should return JSON and reject mutating statements."
  (require 'superchat-tools)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "s1" "user" "hello world")
     (let ((result (superchat-tool-sql "SELECT kind, content FROM tape")))
       (should (string-match-p "\"user\"" result))
       (should (string-match-p "hello world" result)))
     (should-error (superchat-tool-sql "DELETE FROM tape"))
     (should-error (superchat-tool-sql "INSERT INTO tape VALUES (1,2,3,4,5,6)")))))

(ert-deftest test-tool-memory-search ()
  "`superchat-tool-memory-search' should return tape matches as JSON."
  (require 'superchat-tools)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "tool-search-session"))
       (superchat-db-tape-append "tool-search-session" "user" "incremental indexing")
       (superchat-db-tape-append "tool-search-session" "assistant" "use trigram tokenizer")
       (let ((result (superchat-tool-memory-search "trigram")))
         (should (string-match-p "trigram" result))
         (should (string-match-p "tool-search-session" result)))))))

(ert-deftest test-tool-file-history ()
  "`superchat-tool-file-history' should find tool entries mentioning a path."
  (require 'superchat-tools)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((superchat--session-id "tool-file-session"))
       (superchat-db-tape-append
        "tool-file-session" "tool_call" "write-file"
        :meta '((name . "write-file") (args . ((path . "/tmp/init.el")))))
       (superchat-db-tape-append
        "tool-file-session" "tool_result" "wrote /tmp/init.el")
       (let ((result (superchat-tool-file-history "/tmp/init.el")))
         (should (string-match-p "write-file" result))
         (should (string-match-p "/tmp/init.el" result)))))))

(ert-deftest test-memory-migration-creates-anchors ()
  "`superchat-memory-migrate-to-tape' should convert accepted memories to anchors."
  (require 'superchat-memory)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-memory-insert "remember to use lexical-binding"
                                 :title "lexical binding"
                                 :keywords "elisp"
                                 :status "accepted")
     (let ((count (superchat-memory-migrate-to-tape)))
       (should (= count 1))
       (let ((rows (superchat-db-tape-select
                    "SELECT kind, content, topic FROM tape WHERE session_id = ?"
                    (list "memory-migration"))))
         (should (= (length rows) 1))
         (should (string= "anchor" (caar rows)))
         (should (string= "remember to use lexical-binding" (cadar rows)))
         (should (string= "migrated-memory" (caddar rows))))))))

(provide 'test-tape-view)

;;; test-tape-view.el ends here
