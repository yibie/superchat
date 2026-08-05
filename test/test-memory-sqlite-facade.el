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
