;;; test-memory-soul.el — Soul tests surviving v0.8 SQLite migration -*- lexical-binding: t; -*-

;;; Commentary:
;; After the v0.8 SQLite migration, only defcustom presence, extract-text,
;; and add-raw tape behavior survive.

;;; Code:

(require 'ert)
(require 'superchat-memory)
(require 'superchat-db)

(defmacro test-memory-sqlite--with-temp-store (&rest body)
  (declare (indent 0))
  `(let* ((tmp-dir (make-temp-file "superchat-memory-soul" t))
          (superchat-data-directory tmp-dir))
     (unwind-protect
         (progn
           (ignore-errors (superchat-db-close))
           (superchat-db-open)
           ,@body)
       (ignore-errors (superchat-db-close))
       (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest test-memory-soul-defcustoms-exist ()
  (should (boundp 'superchat-memory-auto-capture-enabled))
  (should (boundp 'superchat-memory-auto-capture-minimum-length))
  (should (boundp 'superchat-memory-title-max-length))
  (should (boundp 'superchat-memory-retrieve-limit))
  (should (boundp 'superchat-memory-prune-rejected-days)))

(ert-deftest test-memory-soul-add-raw-creates-tape-entry ()
  (test-memory-sqlite--with-temp-store
    (let ((id (superchat-memory-add-raw
               "User prefers dark themes." :mood "neutral" :context "prefs")))
      (should id))))

(ert-deftest test-memory-soul-add-raw-untagged-mood-falls-back ()
  (test-memory-sqlite--with-temp-store
    (let ((id (superchat-memory-add-raw "Just a thought")))
      (should id))))

(ert-deftest test-memory-soul-extract-response-text-dotted-pair ()
  ;; A dotted cons is not a proper plist — fallback to format.
  (should (stringp (superchat-memory--extract-text (cons :input "hello")))))

(ert-deftest test-memory-soul-extract-response-text-proper-plist-still-works ()
  (should (equal "hi" (superchat-memory--extract-text '(:input "hi" :other 1)))))

(ert-deftest test-memory-soul-extract-response-text-scalar-cases ()
  (should (equal "plain" (superchat-memory--extract-text "plain")))
  ;; Empty list formats as "nil" (via %S)
  (should (stringp (superchat-memory--extract-text '())))
  (should (stringp (superchat-memory--extract-text 42))))

(ert-deftest test-memory-soul-superchat-llm-backend-defvar ()
  (should (special-variable-p 'superchat-llm-backend)))

(provide 'test-memory-soul)
;;; test-memory-soul.el ends here
