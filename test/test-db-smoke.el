;;; test-db-smoke.el — Smoke test for superchat-db -*- lexical-binding: t; -*-

(require 'cl-lib)

(add-to-list 'load-path default-directory)

(message "=== Loading superchat-db... ===")
(require 'superchat-db)
(message "Loaded OK")

(let ((db (superchat-db-open "/tmp/sc-smoke/superchat.db")))
  (message "DB opened")

  ;; Insert test memories
  (superchat-db-memory-insert "User prefers Python over JavaScript"
                               :title "Language preference"
                               :keywords "python,javascript")
  (message "Insert 1 OK: id=%s" (caar (sqlite-select db "SELECT last_insert_rowid()")))

  (superchat-db-memory-insert "User lives in Shanghai, timezone UTC+8"
                               :title "Location"
                               :keywords "shanghai,timezone")
  (message "Insert 2 OK")

  ;; Tape append
  (superchat-db-tape-append "sess-test" "user" "Hello world" :meta '((mood . "curious")))
  (superchat-db-tape-append "sess-test" "assistant" "Hi! How can I help?")
  (message "Tape: %d entries" (length (superchat-db-tape-replay "sess-test")))

  ;; Accept them
  (superchat-db-memory-set-review 1 "accepted")
  (superchat-db-memory-set-review 2 "accepted")
  (message "Reviews set OK")

  ;; Test simple search
  (let ((r (superchat-db-memory-search-simple "python" 5)))
    (message "Search 'python': %d results" (length r))
    (dolist (row r)
      (message "  -> %s" (nth 2 row))))

  ;; Test FTS5 search  
  (let ((r (superchat-db-memory-search "python" 5)))
    (message "FTS5 'python': %d results" (length r)))

  (message "Stats: %S" (superchat-db-stats))
  (superchat-db-close)
  (delete-file "/tmp/sc-smoke/superchat.db")
  (message "=== ALL SMOKE TESTS PASSED ==="))
