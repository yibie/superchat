;;; test-memory-sqlite-facade.el — SQLite memory facade tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive tests for the SQLite-backed memory facade (v0.8).
;; Covers capture, retrieve, prune, title, and tape.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)                ; org-with-wide-buffer etc
(require 'superchat-db)
(require 'superchat-memory)

(defmacro test-memory-sqlite--with-temp-store (&rest body)
  (declare (indent 0))
  `(let* ((tmp-dir (make-temp-file "superchat-memory-sqlite" t))
          (superchat-data-directory tmp-dir))
     (unwind-protect
         (progn
           (ignore-errors (superchat-db-close))
           (superchat-db-open)
           ,@body)
       (ignore-errors (superchat-db-close))
       (ignore-errors (delete-directory tmp-dir t)))))

;; ── explicit capture ──

(ert-deftest test-memory-sqlite-explicit-capture-is-accepted-and-recallable ()
  (test-memory-sqlite--with-temp-store
    (let ((id (superchat-memory-capture-explicit
               "I prefer black coffee when coding." "Coffee preference")))
      (should (integerp id))
      (should (= 1 (superchat-db-memory-count "accepted")))
      (let ((results (superchat-memory-retrieve "black coffee")))
        (should (= 1 (length results)))
        (should (equal "Coffee preference" (plist-get (car results) :title)))
        (should (string-match-p "black coffee" (plist-get (car results) :content)))))))

(ert-deftest test-memory-projection-separates-event-id-from-storage-cursor ()
  (test-memory-sqlite--with-temp-store
    (let* ((memory-id
            (superchat-memory-capture-explicit
             "Logical identity survives export." "Identity"))
           (row (superchat-db-memory-get-by-id memory-id))
           (source-tape-ids
            (let ((json-array-type 'list))
              (json-read-from-string (nth 8 row))))
           (source-event-ids
            (let ((json-array-type 'list))
              (json-read-from-string (nth 9 row)))))
      (should (integerp (car source-tape-ids)))
      (should (string-prefix-p "evt_" (car source-event-ids)))
      (should-not (equal source-tape-ids source-event-ids)))))

(ert-deftest memory-provenance/validates-exact-remember-bridge-chain ()
  "Memory recovers a complete typed bridge relation from durable identities."
  (test-memory-sqlite--with-temp-store
    (let* ((session-id "memory-provenance-valid")
           (source-id
            (superchat-tape-append-material
             session-id "user" "[remember] exact chain"))
           (anchor-id
            (superchat-tape-append-material
             session-id "anchor" "exact chain"
             :topic "remember"
             :meta `((source_ids . [,source-id]))))
           (superchat--session-id session-id)
           (memory-id
            (superchat-memory-capture-explicit
             "exact chain" "Exact chain"
             :remember-session-id session-id
             :remember-anchor-id anchor-id
             :remember-source-id source-id))
           (candidate (car (superchat-memory-retrieve "exact chain")))
           (row (superchat-db-memory-get-by-id memory-id))
           (capture-id
            (car (let ((json-array-type 'list))
                   (json-read-from-string (nth 8 row)))))
           (capture-row
            (car (superchat-db-tape-select
                  "SELECT id, session_id, topic, kind, content, meta, created_at
                   FROM tape WHERE id = ?"
                  (list capture-id))))
           (meta (let ((json-object-type 'alist)
                       (json-array-type 'list)
                       (json-key-type 'symbol))
                   (json-read-from-string (nth 5 capture-row))))
           (relation (alist-get 'remember_bridge
                                (alist-get 'payload meta))))
      (should (equal "remember-anchor-v1"
                     (alist-get 'relation_type relation)))
      (should (equal session-id (alist-get 'session_id relation)))
      (should (= anchor-id (alist-get 'anchor_storage_id relation)))
      (should (= source-id (alist-get 'source_storage_id relation)))
      (should (= anchor-id
                 (superchat-memory-verified-remember-anchor-id
                  candidate session-id))))))

(ert-deftest memory-provenance/malformed-or-unlinked-relations-fail-closed ()
  "Incomplete identities and ordinary Memory never produce an Anchor verdict."
  (test-memory-sqlite--with-temp-store
    (let* ((session-id "memory-provenance-negative")
           (source-id
            (superchat-tape-append-material
             session-id "user" "[remember] negative chain"))
           (anchor-id
            (superchat-tape-append-material
             session-id "anchor" "negative chain"
             :topic "remember"
             :meta `((source_ids . [,source-id]))))
           (superchat--session-id session-id)
           (ordinary-id
            (superchat-memory-capture-explicit "ordinary fact" "Ordinary"))
           (malformed-id
            (superchat-memory-capture-explicit
             "malformed fact" "Malformed"
             :remember-session-id session-id
             :remember-anchor-id 0
             :remember-source-id source-id)))
      (dolist (case `((,ordinary-id ,session-id)
                      (,malformed-id ,session-id)
                      (,ordinary-id "foreign-session")))
        (let* ((row (superchat-db-memory-get-by-id (car case)))
               (candidate (superchat-memory--row-to-plist row)))
          (should-not
           (superchat-memory-verified-remember-anchor-id
            candidate (cadr case)))))
      ;; A structurally complete relation still fails when recall Session differs.
      (let* ((valid-id
              (superchat-memory-capture-explicit
               "session mismatch" "Mismatch"
               :remember-session-id session-id
               :remember-anchor-id anchor-id
               :remember-source-id source-id))
             (candidate
              (superchat-memory--row-to-plist
               (superchat-db-memory-get-by-id valid-id))))
        (should-not
         (superchat-memory-verified-remember-anchor-id
          candidate "foreign-session"))))))

(ert-deftest memory-provenance/late-source-fails-closed ()
  "A source appended after its Anchor and capture cannot verify provenance."
  (test-memory-sqlite--with-temp-store
    (let* ((session-id "memory-provenance-late-source")
           (anchor-id
            (superchat-tape-append-material
             session-id "anchor" "late source" :topic "remember"
             :meta '((source_ids . [3]))))
           (capture
            (superchat-tape-append-event-record
             `(:type "memory.captured" :session-id ,session-id
               :topic "memory" :summary "memory.captured: Late source"
               :sensitivity "confidential" :requires-payload t
               :payload
               (:content "late source" :title "Late source"
                :keywords "" :mood "" :status "accepted"
                :model_id "" :prompt_hash ""
                :remember_bridge
                (:relation_type "remember-anchor-v1"
                 :session_id ,session-id
                 :anchor_storage_id ,anchor-id
                 :source_storage_id 3)))))
           (capture-id (plist-get capture :storage-id))
           (source-id
            (superchat-tape-append-material session-id "user" "late source")))
      (should (= 1 anchor-id))
      (should (= 2 capture-id))
      (should (= 3 source-id))
      (superchat-projection-catch-up 'memory)
      (let* ((memory-id
              (car (superchat-db-memory-get-by-source-event
                    (plist-get capture :event-id))))
             (candidate
              (superchat-memory--row-to-plist
               (superchat-db-memory-get-by-id memory-id))))
        (should-not
         (superchat-memory-verified-remember-anchor-id
          candidate session-id))))))

(ert-deftest memory-provenance/identity-and-anchor-mutations-fail-closed ()
  "Stored Session, event, Anchor, and source mismatches preserve Memory."
  (test-memory-sqlite--with-temp-store
    (let* ((session-id "memory-provenance-mutations")
           (source-id
            (superchat-tape-append-material session-id "user" "source"))
           (anchor-id
            (superchat-tape-append-material
             session-id "anchor" "mutation fact" :topic "remember"
             :meta `((source_ids . [,source-id]))))
           (superchat--session-id session-id)
           (memory-id
            (superchat-memory-capture-explicit
             "mutation fact" "Mutation"
             :remember-session-id session-id
             :remember-anchor-id anchor-id
             :remember-source-id source-id))
           (candidate
            (superchat-memory--row-to-plist
             (superchat-db-memory-get-by-id memory-id)))
           (db (superchat-db-open)))
      (should (= anchor-id
                 (superchat-memory-verified-remember-anchor-id
                  candidate session-id)))
      (dolist (mutation
               `(("UPDATE tape SET topic = 'wrong' WHERE id = ?"
                  "UPDATE tape SET topic = 'remember' WHERE id = ?" ,anchor-id)
                 ("UPDATE tape SET kind = 'user' WHERE id = ?"
                  "UPDATE tape SET kind = 'anchor' WHERE id = ?" ,anchor-id)
                 ("UPDATE tape SET session_id = 'foreign' WHERE id = ?"
                  ,(format "UPDATE tape SET session_id = '%s' WHERE id = ?"
                           session-id)
                  ,anchor-id)
                 ("UPDATE tape SET session_id = 'foreign' WHERE id = ?"
                  ,(format "UPDATE tape SET session_id = '%s' WHERE id = ?"
                           session-id)
                  ,source-id)
                 ("UPDATE tape SET meta = '{\"source_ids\":[]}' WHERE id = ?"
                  ,(format "UPDATE tape SET meta = '{\"source_ids\":[%d]}' WHERE id = ?"
                           source-id)
                  ,anchor-id)))
        (sqlite-execute db (nth 0 mutation) (list (nth 2 mutation)))
        (should-not
         (superchat-memory-verified-remember-anchor-id candidate session-id))
        (sqlite-execute db (nth 1 mutation) (list (nth 2 mutation))))
      (let ((old-events (nth 9 (superchat-db-memory-get-by-id memory-id))))
        (sqlite-execute db
                        "UPDATE memory SET source_event_ids = '[\"wrong\"]' WHERE id = ?"
                        (list memory-id))
        (should-not
         (superchat-memory-verified-remember-anchor-id candidate session-id))
        (sqlite-execute db
                        "UPDATE memory SET source_event_ids = ? WHERE id = ?"
                        (list old-events memory-id)))
      (sqlite-execute db "DELETE FROM tape WHERE id = ?" (list source-id))
      (should-not
       (superchat-memory-verified-remember-anchor-id candidate session-id)))))

(ert-deftest memory-provenance/correction-does-not-inherit-bridge-relation ()
  "A correction is a new Memory fact with no predecessor bridge relation."
  (test-memory-sqlite--with-temp-store
    (let* ((session-id "memory-provenance-correction")
           (source-id
            (superchat-tape-append-material session-id "user" "source"))
           (anchor-id
            (superchat-tape-append-material
             session-id "anchor" "original" :topic "remember"
             :meta `((source_ids . [,source-id]))))
           (superchat--session-id session-id)
           (memory-id
            (superchat-memory-capture-explicit
             "original" "Original"
             :remember-session-id session-id
             :remember-anchor-id anchor-id
             :remember-source-id source-id))
           (replacement-id
            (superchat-memory-correct memory-id "corrected" "Corrected"))
           (replacement
            (superchat-memory--row-to-plist
             (superchat-db-memory-get-by-id replacement-id))))
      (should-not
       (superchat-memory-verified-remember-anchor-id replacement session-id))
      (should (equal "accepted"
                     (nth 5 (superchat-db-memory-get-by-id memory-id))))
      (should (= replacement-id
                 (nth 7 (superchat-db-memory-get-by-id memory-id)))))))

(ert-deftest memory-provenance/stored-envelope-mismatches-fail-closed ()
  "Malformed relation fields, missing rows, and capture mismatch return nil."
  (test-memory-sqlite--with-temp-store
    (let* ((session-id "memory-provenance-envelope")
           (source-id
            (superchat-tape-append-material session-id "user" "source"))
           (anchor-id
            (superchat-tape-append-material
             session-id "anchor" "envelope fact" :topic "remember"
             :meta `((source_ids . [,source-id]))))
           (superchat--session-id session-id)
           (memory-id
            (superchat-memory-capture-explicit
             "envelope fact" "Envelope"
             :remember-session-id session-id
             :remember-anchor-id anchor-id
             :remember-source-id source-id))
           (candidate
            (superchat-memory--row-to-plist
             (superchat-db-memory-get-by-id memory-id)))
           (capture-id
            (car (let ((json-array-type 'list))
                   (json-read-from-string
                    (nth 8 (superchat-db-memory-get-by-id memory-id))))))
           (db (superchat-db-open))
           (original-meta
            (caar (sqlite-select db "SELECT meta FROM tape WHERE id = ?"
                                 (list capture-id)))))
      (dolist (rewrite
               (list
                (lambda (meta)
                  (setf (alist-get 'remember_bridge
                                   (alist-get 'payload meta) nil 'remove)
                        nil)
                  meta)
                (lambda (meta)
                  (setf (alist-get 'anchor_storage_id
                                   (alist-get 'remember_bridge
                                              (alist-get 'payload meta)))
                        "not-an-id")
                  meta)
                (lambda (meta)
                  (setf (alist-get 'session_id
                                   (alist-get 'remember_bridge
                                              (alist-get 'payload meta)))
                        "foreign")
                  meta)
                (lambda (meta)
                  (setf (alist-get 'anchor_storage_id
                                   (alist-get 'remember_bridge
                                              (alist-get 'payload meta)))
                        999999)
                  meta)
                (lambda (meta)
                  (setf (alist-get 'event_type meta) "memory.corrected")
                  meta)))
        (let ((meta (let ((json-object-type 'alist)
                          (json-array-type 'list)
                          (json-key-type 'symbol))
                      (json-read-from-string original-meta))))
          (sqlite-execute db "UPDATE tape SET meta = ? WHERE id = ?"
                          (list (json-encode (funcall rewrite meta)) capture-id))
          (should-not
           (superchat-memory-verified-remember-anchor-id
            candidate session-id))))
      (sqlite-execute db "UPDATE tape SET meta = '{' WHERE id = ?"
                      (list capture-id))
      (should-not
       (superchat-memory-verified-remember-anchor-id candidate session-id)))))

(ert-deftest test-memory-projection-stops-checkpoint-at-malformed-meta ()
  "A bad durable event blocks later Memory events without duplicating prefix."
  (test-memory-sqlite--with-temp-store
    (let* ((first
            (superchat-tape-append-event-record
             '(:type "memory.captured" :session-id "memory-a"
               :topic "memory"
               :payload (:content "first fact" :title "First"
                         :status "accepted"))))
           (first-id (plist-get first :storage-id)))
      (sqlite-execute
       (superchat-db-open)
       "INSERT INTO tape (session_id, topic, kind, content, meta)
        VALUES (?, 'memory', 'system', 'malformed', '{')"
       '("memory-bad"))
      (superchat-tape-append-event
       '(:type "memory.captured" :session-id "memory-b"
         :topic "memory"
         :payload (:content "later fact" :title "Later"
                   :status "accepted")))
      (should-error (superchat-projection-catch-up 'memory))
      (should (equal (list 1 first-id)
                     (superchat-db-projection-checkpoint 'memory)))
      (should (= 1 (superchat-db-memory-count)))
      (should (equal "First"
                     (nth 1 (car (superchat-db-memory-search-simple
                                  "first fact")))))
      (should-not (superchat-db-memory-search-simple "later fact"))
      (should-error (superchat-projection-catch-up 'memory))
      (should (= 1 (superchat-db-memory-count)))
      (should-error (superchat-projection-rebuild 'memory))
      (should (equal (list 1 first-id)
                     (superchat-db-projection-checkpoint 'memory)))
      (should (= 1 (superchat-db-memory-count))))))

(ert-deftest test-memory-sqlite-explicit-capture-with-keywords ()
  (test-memory-sqlite--with-temp-store
    (superchat-memory-capture-explicit
     "Rust's borrow checker prevents data races." "Borrow checker"
     :keywords "rust,safety")
    (let ((results (superchat-memory-retrieve "rust")))
      (should (= 1 (length results)))
      (should (string-match-p "rust" (or (plist-get (car results) :keywords) ""))))))

;; ── conversation capture ──

(ert-deftest test-memory-sqlite-conversation-capture ()
  (test-memory-sqlite--with-temp-store
    (let ((id (superchat-memory-capture-conversation
               '(:input "Explain Lisp macros" :user "What are macros?"))))
      (should (integerp id))
      (should (= 1 (superchat-db-memory-count "accepted"))))))

(ert-deftest test-memory-sqlite-conversation-capture-string-input ()
  (test-memory-sqlite--with-temp-store
    (should (integerp (superchat-memory-capture-conversation "Plain string input")))))

;; ── auto capture ──

(ert-deftest test-memory-sqlite-auto-capture-triggered-by-pattern ()
  (let ((superchat-memory-auto-capture-enabled t)
        (superchat-memory-auto-capture-minimum-length 3))
    (test-memory-sqlite--with-temp-store
      (let ((id (superchat-memory-auto-capture "remember this")))
        (should (integerp id))))))

(ert-deftest test-memory-sqlite-auto-capture-skipped-below-length-floor ()
  (let ((superchat-memory-auto-capture-enabled t)
        (superchat-memory-auto-capture-minimum-length 999))
    (test-memory-sqlite--with-temp-store
      (should-not (superchat-memory-auto-capture "remember ok")))))

(ert-deftest test-memory-sqlite-auto-capture-disabled ()
  (let ((superchat-memory-auto-capture-enabled nil))
    (test-memory-sqlite--with-temp-store
      (should-not (superchat-memory-auto-capture "remember this")))))

;; ── retrieve ──

(ert-deftest test-memory-sqlite-retrieve-fts5-hit ()
  (test-memory-sqlite--with-temp-store
    (superchat-memory-capture-explicit "Emacs Lisp is a dialect of Lisp." "Emacs Lisp")
    (superchat-memory-capture-explicit "Python uses significant whitespace." "Python")
    (let ((results (superchat-memory-retrieve "Lisp")))
      (should (= 1 (length results)))
      (should (equal "Emacs Lisp" (plist-get (car results) :title))))))

(ert-deftest test-memory-sqlite-retrieve-fts5-miss ()
  (test-memory-sqlite--with-temp-store
    (superchat-memory-capture-explicit "I like tea." "Tea")
    (should-not (superchat-memory-retrieve "xyznonexistent"))))

(ert-deftest test-memory-sqlite-retrieve-limit-honored ()
  (let ((superchat-memory-retrieve-limit 2))
    (test-memory-sqlite--with-temp-store
      (dotimes (i 5)
        (superchat-memory-capture-explicit (format "Memory item %d" i) (format "Item %d" i)))
      (let ((results (superchat-memory-retrieve "item")))
        (should (<= (length results) 2))))))

(ert-deftest test-memory-sqlite-pending-is-not-recalled ()
  (test-memory-sqlite--with-temp-store
    (let ((id
           (superchat-memory-summarize-session-history
            "Pending session detail that requires review.")))
      (should (integerp id))
      (should-not (superchat-memory-retrieve "Pending session detail"))
      (superchat-memory-review id 'accepted)
      (should (= 1 (length
                    (superchat-memory-retrieve
                     "Pending session detail")))))))

(ert-deftest test-memory-sqlite-review-and-correction-survive-rebuild ()
  (test-memory-sqlite--with-temp-store
    (let* ((legacy-id
            (superchat-db-memory-insert
             "Legacy durable row." :title "Legacy" :status "accepted"))
           (original-id
            (superchat-memory-capture-explicit
             "The project uses the old parser." "Parser fact"))
           (rejected-id
            (superchat-memory-capture-explicit
             "Temporary incorrect preference." "Temporary")))
      (superchat-memory-review rejected-id 'rejected)
      (superchat-memory-correct
       original-id "The project uses the incremental parser." "Parser fact")
      (let ((before
             (superchat-db-tape-select
              "SELECT title, content, review_status,
                      CASE WHEN replaced_by IS NULL THEN 0 ELSE 1 END,
                      source_event_ids
               FROM memory
               WHERE projection_version = 1
               ORDER BY source_event_ids")))
        (superchat-projection-rebuild 'memory)
        (should
         (equal
          before
          (superchat-db-tape-select
           "SELECT title, content, review_status,
                   CASE WHEN replaced_by IS NULL THEN 0 ELSE 1 END,
                   source_event_ids
            FROM memory
            WHERE projection_version = 1
            ORDER BY source_event_ids")))
        (should (superchat-db-memory-get-by-id legacy-id))
        (should-not (superchat-memory-retrieve "old parser"))
        (should (= 1 (length
                      (superchat-memory-retrieve
                       "incremental parser"))))
        (should-not
         (superchat-memory-retrieve "incorrect preference"))))))

;; ── title ──

(ert-deftest test-memory-sqlite-compose-title-length-cap ()
  (let ((superchat-memory-title-max-length 10))
    (let ((title (superchat-memory-compose-title "This is a very long content string")))
      (should (<= (length title) 11)))))

;; ── tape ──

(ert-deftest test-memory-sqlite-tape-append ()
  (test-memory-sqlite--with-temp-store
    (let ((id (superchat-memory-add-raw "Log entry" :mood "neutral" :verbatim t)))
      (should id))))

(provide 'test-memory-sqlite-facade)
;;; test-memory-sqlite-facade.el ends here

;; ── import ──

(ert-deftest test-memory-sqlite-import-from-org-roundtrip ()
  (test-memory-sqlite--with-temp-store
    (let ((org-file (expand-file-name "memory.org" superchat-data-directory)))
      (with-temp-buffer
        (insert "* Coffee preference\n:PROPERTIES:\n:KEYWORDS: coffee\n:END:\nI prefer black coffee.\n")
        (write-region (point-min) (point-max) org-file))
      (let ((count (superchat-memory-import-from-org org-file)))
        (should (= 1 count))
        (should (= 1 (superchat-db-memory-count "accepted")))
        (let ((results (superchat-memory-retrieve "coffee")))
          (should (= 1 (length results)))
          (should (equal "Coffee preference" (plist-get (car results) :title))))))))
