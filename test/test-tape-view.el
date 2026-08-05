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

(defun test-tape-view--append-config-event (session-id type payload)
  "Append a Runtime config event envelope to SESSION-ID."
  (superchat-db-tape-append
   session-id "system" type
   :meta `((event_type . ,type)
           (schema . 1)
           (payload . ,payload))))

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

(ert-deftest test-tape-v2-to-v4-migration ()
  "Opening a schema-v2 database must migrate through v4."
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
          (should (= 4 (superchat-db--current-schema-version db)))
          (should (cl-some (lambda (col) (string= "topic" (cadr col)))
                           (sqlite-select db "PRAGMA table_info(tape)")))
          (should (cl-some
                   (lambda (col) (string= "source_event_ids" (cadr col)))
                   (sqlite-select db "PRAGMA table_info(memory)")))
          (should (sqlite-select
                   db
                   "SELECT 1 FROM sqlite_master WHERE type='table' AND name='tape_fts'"))
          (should (sqlite-select
                   db
                   "SELECT 1 FROM sqlite_master
                    WHERE type='table' AND name='projection_checkpoint'"))
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

(ert-deftest view/session-history-follows-branch-lineage ()
  "A branch view should inherit parent history only through its branch point."
  (test-tape-view--with-temp-db
   (lambda ()
     (let* ((root-user (superchat-db-tape-append "root" "user" "question"))
            (root-answer (superchat-db-tape-append
                          "root" "assistant" "first answer")))
       (superchat-db-tape-append "root" "user" "later parent turn")
       (superchat-db-tape-append
        "branch" "anchor" "Branched from root"
        :topic "branch"
        :meta `((parent_session_id . "root")
                (parent_turn_id . ,root-answer)
                (source_ids . [,root-user ,root-answer])))
       (superchat-db-tape-append "branch" "user" "branch question")
       (let* ((rows (superchat-view-session-history "branch"))
              (contents (mapcar (lambda (row) (nth 3 row)) rows)))
         (should (equal contents
                        '("question" "first answer"
                          "Branched from root" "branch question")))
         (should-not (member "later parent turn" contents)))))))

(ert-deftest view/session-configuration-defaults ()
  "A Session with no config events should use chat defaults."
  (test-tape-view--with-temp-db
   (lambda ()
     (should (equal (superchat-view-session-configuration "empty")
                    '(:capability "chat"
                      :capability-source nil
                      :topology "single"
                      :authority-policy "default"
                      :workspace-id nil
                      :workspace-root nil
                      :surface nil))))))

(ert-deftest view/session-configuration-replays-latest-events ()
  "Configuration replay should project the latest event envelopes."
  (test-tape-view--with-temp-db
   (lambda ()
     (test-tape-view--append-config-event
      "config" "capability.activated"
      '((capability . "agent") (capability_source . "preset")))
     (test-tape-view--append-config-event
      "config" "topology.changed" '((topology . "team")))
     (test-tape-view--append-config-event
      "config" "authority.policy.selected" '((policy . "confirm-risky")))
     (test-tape-view--append-config-event
      "config" "workspace.bound"
      '((workspace_id . "workspace-1") (workspace_root . "/repo")))
     (test-tape-view--append-config-event
      "config" "intent.received" '((surface . "magit")))
     (should (equal (superchat-view-session-configuration "config")
                    '(:capability "agent"
                      :capability-source "preset"
                      :topology "team"
                      :authority-policy "confirm-risky"
                      :workspace-id "workspace-1"
                      :workspace-root "/repo"
                      :surface "magit")))
     (test-tape-view--append-config-event
      "config" "workspace.unbound" nil)
     (test-tape-view--append-config-event
      "config" "intent.received" '((content_size . 12)))
     (should (equal (superchat-view-session-configuration "config")
                    '(:capability "agent"
                      :capability-source "preset"
                      :topology "team"
                      :authority-policy "confirm-risky"
                      :workspace-id nil
                      :workspace-root nil
                      :surface "magit"))))))

(ert-deftest view/session-configuration-projects-and-clears-profile ()
  "Capability changes project Profile identity without retaining stale data."
  (test-tape-view--with-temp-db
   (lambda ()
     (test-tape-view--append-config-event
      "profile-config" "capability.activated"
      '((name . "coding")
        (profile_id . "coder-safe")
        (source . "skill")))
     (let ((config
            (superchat-view-session-configuration "profile-config")))
       (should (equal "coding" (plist-get config :capability)))
       (should (equal "coder-safe" (plist-get config :profile-id))))
     (test-tape-view--append-config-event
      "profile-config" "capability.activated" '((name . "chat")))
     (should-not
      (plist-member
       (superchat-view-session-configuration "profile-config")
       :profile-id)))))

(ert-deftest view/session-configuration-inherits-branch-history ()
  "Branch Sessions should inherit config through their branch point."
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((capability-id
            (test-tape-view--append-config-event
             "root-config" "capability.activated"
             '((capability . "agent") (source . "parent")))))
       (test-tape-view--append-config-event
        "root-config" "capability.activated" '((capability . "chat")))
       (superchat-db-tape-append
        "child-config" "anchor" "Branched from root-config"
        :topic "branch"
        :meta `((parent_session_id . "root-config")
                (parent_turn_id . ,capability-id)))
       (test-tape-view--append-config-event
        "child-config" "topology.changed" '((topology . "team")))
       (should (equal (superchat-view-session-configuration "child-config")
                      '(:capability "agent"
                        :capability-source "parent"
                        :topology "team"
                        :authority-policy "default"
                        :workspace-id nil
                        :workspace-root nil
                        :surface nil)))))))

(ert-deftest view/session-configuration-ignores-malformed-meta ()
  "Malformed or legacy meta should not affect projected config."
  (test-tape-view--with-temp-db
   (lambda ()
     (test-tape-view--append-config-event
      "malformed" "capability.activated" '((capability . "agent")))
     (sqlite-execute
      (superchat-db-open)
      "INSERT INTO tape(session_id, kind, content, meta) VALUES (?, ?, ?, ?)"
      (list "malformed" "system" "bad json" "{bad json"))
     (superchat-db-tape-append
      "malformed" "system" "bad payload"
      :meta '((event_type . "capability.activated")
              (payload . "not an object")))
     (superchat-db-tape-append
      "malformed" "system" "legacy meta" :meta '((capability . "chat")))
     (should (equal (superchat-view-session-configuration "malformed")
                    '(:capability "agent"
                      :capability-source nil
                      :topology "single"
                      :authority-policy "default"
                      :workspace-id nil
                      :workspace-root nil
                      :surface nil))))))

(ert-deftest view/session-ids-are-most-recent-first ()
  "Session discovery should be derived from Tape activity."
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "older" "user" "one")
     (superchat-db-tape-append "newer" "user" "two")
     (should (equal (superchat-view-session-ids) '("newer" "older"))))))

(ert-deftest view/open-session-replays-into-independent-buffer ()
  "Opening a Session should reconstruct a usable conversation View."
  (require 'superchat)
  (let ((workspace (make-temp-file "superchat-replay-workspace" t)))
    (unwind-protect
        (test-tape-view--with-temp-db
         (lambda ()
           (test-tape-view--append-config-event
            "replay-session" "capability.activated"
            '((name . "code") (source . "builtin")))
           (test-tape-view--append-config-event
            "replay-session" "topology.changed" '((topology . "team")))
           (test-tape-view--append-config-event
            "replay-session" "authority.policy.selected"
            '((policy . "observer")))
           (test-tape-view--append-config-event
            "replay-session" "workspace.bound"
            `((workspace_id . "workspace-replay")
              (workspace_root . ,workspace)))
           (superchat-db-tape-append "replay-session" "user" "hello")
           (superchat-db-tape-append "replay-session" "assistant" "world")
           (let ((buffer (superchat-open-session "replay-session")))
             (unwind-protect
                 (with-current-buffer buffer
                   (should-not (equal (buffer-name) superchat-buffer-name))
                   (should (equal superchat--session-id "replay-session"))
                   (should (equal
                            "code"
                            (superchat-preset-name
                             superchat--active-preset)))
                   (should (eq 'team superchat--active-topology))
                   (should (equal "observer"
                                  superchat--active-authority-policy))
                   (should (equal "workspace-replay"
                                  (plist-get superchat--workspace-binding
                                             :id)))
                   (should (= (length superchat--conversation-history) 2))
                   (should (string-match-p "User: hello" (buffer-string)))
                   (should (string-match-p "Assistant" (buffer-string)))
                   (should (marker-position superchat--prompt-start))
                   (goto-char (point-max))
                   (insert "next question")
                   (should (string-suffix-p
                            "next question" (buffer-string))))
               (when (buffer-live-p buffer)
                 (kill-buffer buffer))))))
      (delete-directory workspace t))))

(ert-deftest view/image-attachment-persists-as-metadata ()
  "Tape stores image metadata and reopened Views render the attachment."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((image (make-temp-file "superchat-tape-image-" nil ".jpg"))
           buffer)
       (unwind-protect
           (progn
             (with-temp-file image (insert "JPEG"))
             (let ((turn (superchat-turn--create
                          :id "media-turn"
                          :session-id "media-session"
                          :inbound "Describe image"
                          :context-files (list image))))
               (superchat--hook-record-attachments turn))
             (let ((row (car (superchat-db-tape-replay "media-session"))))
               (should (equal (nth 1 row) "attachment"))
               (should (equal (nth 2 row) "system"))
               (should (string-match-p "image/jpeg, 4 bytes" (nth 3 row)))
               (let ((json-key-type 'symbol))
                 (let* ((meta (json-read-from-string (nth 4 row)))
                        (payload (cdr (assq 'payload meta))))
                   (should (equal "attachment.added"
                                  (cdr (assq 'event_type meta))))
                   (should (string-prefix-p
                            "evt_" (cdr (assq 'event_id meta))))
                   (should (equal (cdr (assq 'path payload)) image))
                   (should-not (assq 'data payload)))))
             (setq buffer (superchat-open-session "media-session"))
             (with-current-buffer buffer
               (should (string-match-p "Attachment" (buffer-string)))
               (should (string-match-p "image/jpeg, 4 bytes"
                                       (buffer-string)))))
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (delete-file image))))))

(ert-deftest view/new-sessions-own-independent-buffers ()
  "New Sessions should not share their Buffer or buffer-local state."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (let ((ids '("new-session-a" "new-session-b"))
           (superchat-display-single-window nil)
           first second)
       (cl-letf (((symbol-function 'superchat--new-session-id)
                  (lambda () (pop ids))))
         (setq first (superchat-new-session)
               second (superchat-new-session)))
       (unwind-protect
           (progn
             (should-not (eq first second))
             (with-current-buffer first
               (should (equal superchat--session-id "new-session-a"))
               (setq superchat--conversation-history '((:role "user"))))
             (with-current-buffer second
               (should (equal superchat--session-id "new-session-b"))
               (should-not superchat--conversation-history)))
         (when (buffer-live-p first)
           (kill-buffer first))
         (when (buffer-live-p second)
           (kill-buffer second)))))))

(ert-deftest view/branch-session-persists-lineage-and-opens-child ()
  "Branching should persist one lineage anchor and open the child View."
  (require 'superchat)
  (test-tape-view--with-temp-db
   (lambda ()
     (superchat-db-tape-append "branch-root" "user" "question")
     (let* ((answer-id (superchat-db-tape-append
                        "branch-root" "assistant" "answer"))
            (parent-buffer (superchat-open-session "branch-root"))
            child-buffer)
       (unwind-protect
           (progn
             (setq child-buffer
                   (with-current-buffer parent-buffer
                     (superchat-branch-session answer-id)))
             (with-current-buffer child-buffer
               (should-not (equal superchat--session-id "branch-root"))
               (should (equal (mapcar (lambda (row) (nth 3 row))
                                      (superchat-view-session-history
                                       superchat--session-id))
                              '("question" "answer"
                                "Branched from branch-root")))
               (let* ((row
                       (car
                        (superchat-db-tape-replay superchat--session-id)))
                      (json-key-type 'symbol)
                      (meta (json-read-from-string (nth 4 row))))
                 (should (equal "anchor.created"
                                (cdr (assq 'event_type meta))))
                 (should (string-prefix-p
                          "evt_" (cdr (assq 'event_id meta)))))
               (should (string-match-p "User: question" (buffer-string)))
               (should (marker-position superchat--prompt-start))))
         (when (buffer-live-p parent-buffer)
           (kill-buffer parent-buffer))
         (when (buffer-live-p child-buffer)
           (kill-buffer child-buffer)))))))

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
