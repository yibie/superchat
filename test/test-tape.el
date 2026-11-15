;;; test-tape.el --- tests for superchat-tape -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)

(defun superchat-test-tape--texts (entries)
  (mapcar (lambda (e) (plist-get e :text)) entries))

(ert-deftest superchat-tape-append-and-read-back ()
  (superchat-test-with-tape
    (let* ((first (superchat-tape-append "t1" 'user "hello" :scope "/p/"))
           (second (superchat-tape-append
                    "t1" 'assistant "hi"
                    :refs (list (plist-get first :id)) :at 12.5
                    :data '(:model "m" :calls ((:id "c1" :name "read_file" :args (:path "x")))))))
      (should (equal (superchat-tape-get (plist-get first :id)) first))
      (should (equal (plist-get first :kind) 'user))
      (should (equal (plist-get second :refs) (list (plist-get first :id))))
      (should (equal (plist-get second :at) 12.5))
      (should (equal (plist-get (plist-get second :data) :model) "m"))
      (should (equal (plist-get (car (plist-get (plist-get second :data) :calls)) :name)
                     "read_file"))
      (should (equal (superchat-tape-get (plist-get second :id)) second))
      (should-not (superchat-tape-get 9999)))))

(ert-deftest superchat-tape-data-list-shapes-round-trip ()
  (superchat-test-with-tape
    (let ((data '(:nums (1 2 3) :m (("a" "b")) :none nil
                  :ps ((:x 1) (:x 2)) :s ("p" "q") :deep (:in (:l (4 5))))))
      (should (equal (plist-get (superchat-tape-append "t" 'note "x" :scope "home" :data data)
                                :data)
                     data)))))

(ert-deftest superchat-tape-kind-is-a-symbol ()
  (superchat-test-with-tape
    (should-error (superchat-tape-append "t" "user" "x" :scope "home"))
    (should-error (superchat-tape-append "t" 1 "x" :scope "home"))))

(ert-deftest superchat-tape-scope-defaults-to-timeline ()
  (superchat-test-with-tape
    (should-error (superchat-tape-append "t1" 'user "no scope"))
    (superchat-tape-append "t1" 'user "a" :scope "/p/")
    (should (equal (plist-get (superchat-tape-append "t1" 'user "b") :scope) "/p/"))
    (should (equal (plist-get (superchat-tape-append "t1" 'user "c" :scope "home") :scope)
                   "home"))))

(ert-deftest superchat-tape-rejects-unknown-kind ()
  (superchat-test-with-tape
    (should-error (superchat-tape-append "t1" 'bogus "x" :scope "home"))
    (should-not (superchat-tape-entries))))

(ert-deftest superchat-tape-entries-filters ()
  (superchat-test-with-tape
    (let (ids)
      (dolist (spec '(("a" user "1" "/p/") ("a" assistant "2" "/p/") ("b" user "3" "home")
                      ("a" note "4" "/p/") ("a" user "5" "/p/")))
        (push (plist-get (superchat-tape-append (nth 0 spec) (nth 1 spec) (nth 2 spec)
                                                :scope (nth 3 spec))
                         :id)
              ids))
      (setq ids (nreverse ids))
      (should (equal (superchat-test-tape--texts (superchat-tape-entries :tape "a"))
                     '("1" "2" "4" "5")))
      (should (equal (superchat-test-tape--texts (superchat-tape-entries :scope "home")) '("3")))
      (should (equal (superchat-test-tape--texts
                      (superchat-tape-entries :kinds '(user note)))
                     '("1" "3" "4" "5")))
      (should (equal (superchat-test-tape--texts
                      (superchat-tape-entries :after (nth 1 ids) :before (nth 4 ids)))
                     '("3" "4")))
      (should (equal (superchat-test-tape--texts (superchat-tape-entries :limit 2)) '("1" "2")))
      (should (equal (superchat-test-tape--texts
                      (superchat-tape-entries :limit 2 :newest t))
                     '("4" "5"))))))

(ert-deftest superchat-tape-search-finds-words-and-phrases ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "the quick brown fox" :scope "/p/")
    (superchat-tape-append "t" 'user "今天讨论数据库设计" :scope "/p/")
    (superchat-tape-append "t" 'note "another fox elsewhere" :scope "home")
    (should (equal (superchat-test-tape--texts (superchat-tape-search "quick"))
                   '("the quick brown fox")))
    (should (= 2 (length (superchat-tape-search "fox"))))
    (should (equal (superchat-test-tape--texts (superchat-tape-search "fox" :scope "home"))
                   '("another fox elsewhere")))
    (should (equal (superchat-test-tape--texts (superchat-tape-search "fox" :kinds '(user)))
                   '("the quick brown fox")))
    (should (= 1 (length (superchat-tape-search "fox" :limit 1))))
    (should (equal (superchat-test-tape--texts (superchat-tape-search "数据库设计"))
                   '("今天讨论数据库设计")))))

(ert-deftest superchat-tape-search-short-query ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "数据库" :scope "home")
    (superchat-tape-append "t" 'user "100% sure" :scope "home")
    (superchat-tape-append "t" 'user "plain" :scope "home")
    (should (equal (superchat-test-tape--texts (superchat-tape-search "数据")) '("数据库")))
    (should (equal (superchat-test-tape--texts (superchat-tape-search "%")) '("100% sure")))))

(ert-deftest superchat-tape-search-punctuation-is-plain-text ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "see \"a-b\" (c) now" :scope "home")
    (superchat-tape-append "t" 'user "a b c" :scope "home")
    (should (equal (superchat-test-tape--texts (superchat-tape-search "\"a-b\" (c)"))
                   '("see \"a-b\" (c) now")))
    (should-not (superchat-tape-search "NOT OR AND*"))))

(ert-deftest superchat-tape-database-refuses-update-and-delete ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "keep" :scope "home")
    (let ((db (superchat-tape-open)))
      (should-error (sqlite-execute db "UPDATE entry SET text = 'x'"))
      (should-error (sqlite-execute db "DELETE FROM entry"))
      (should (equal (superchat-test-tape--texts (superchat-tape-entries)) '("keep"))))))

(ert-deftest superchat-tape-select-reads-but-cannot-write ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "one" :scope "home")
    (should (equal (superchat-tape-select "SELECT text FROM entry WHERE kind = ?" '("user"))
                   '(("one"))))
    (should (equal (superchat-tape-select "WITH x AS (SELECT 1) SELECT * FROM x") '((1))))
    (should-error (superchat-tape-select
                   "INSERT INTO entry (tape, kind, scope, at) VALUES ('t','user','home',1)"))
    (should-error (superchat-tape-select
                   "WITH x AS (SELECT 1) INSERT INTO entry (tape, kind, scope, at) VALUES ('t','user','home',1)"))
    (should (= 1 (length (superchat-tape-entries))))))

(ert-deftest superchat-tape-new-name-is-fresh ()
  (superchat-test-with-tape
    (let ((names (cl-loop repeat 50 collect (superchat-tape-new-name))))
      (should (= 50 (length (delete-dups (copy-sequence names)))))
      (should (string-match-p "\\`[0-9]\\{8\\}-[0-9]\\{6\\}-[0-9a-f]\\{4\\}\\'" (car names))))))

(ert-deftest superchat-tape-new-name-is-not-a-timeline-already-there ()
  (superchat-test-with-tape
    (let ((draws (list 0 0 1)))
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "20260101-000000"))
                ((symbol-function 'random) (lambda (&rest _) (pop draws))))
        (superchat-tape-append (superchat-tape-new-name) 'user "x" :scope "home")
        (should (equal (superchat-tape-new-name) "20260101-000000-0001"))))))

(ert-deftest superchat-tape-scope-of-project-and-not ()
  (let* ((root (file-name-as-directory (make-temp-file "scope-" t)))
         (project-find-functions (list (lambda (_) (cons 'transient root)))))
    (unwind-protect
        (should (equal (superchat-tape-scope-of root) root))
      (delete-directory root t)))
  (let ((project-find-functions nil))
    (should (equal (superchat-tape-scope-of temporary-file-directory) "home"))))

(ert-deftest superchat-test-tape-select-with-no-row-is-nil ()
  (superchat-test-with-tape
    (should-not (superchat-tape-select "SELECT id FROM entry WHERE id = ?" '(999)))))

(ert-deftest superchat-tape-select-keeps-its-read-only-connection ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "one" :scope "home")
    (let ((opens 0) (open (symbol-function 'sqlite-open)))
      (cl-letf (((symbol-function 'sqlite-open)
                 (lambda (&rest args) (cl-incf opens) (apply open args))))
        (dotimes (_ 5) (superchat-tape-select "SELECT count(*) FROM entry")))
      (should (<= opens 1))
      (should-error (superchat-tape-select
                     "WITH x AS (SELECT 1) INSERT INTO entry (tape, kind, scope, at) VALUES ('t','user','home',1)")))))

(ert-deftest superchat-tape-select-opens-the-tape-when-it-is-not-open ()
  (let* ((dir (file-name-as-directory (make-temp-file "superchat-sel-" t)))
         (superchat-directory (expand-file-name "fresh/" dir)) (superchat-tape-file nil))
    (unwind-protect
        (progn (superchat-tape-close)
               (should-not (superchat-tape-select "SELECT id FROM entry")))
      (superchat-tape-close)
      (delete-directory dir t))))

(ert-deftest superchat-tape-select-never-reads-a-file-the-tape-no-longer-uses ()
  (let* ((dir (file-name-as-directory (make-temp-file "superchat-sel-" t)))
         (a (expand-file-name "a/" dir)) (b (expand-file-name "b/" dir))
         (superchat-directory a) (superchat-tape-file nil))
    (unwind-protect
        (progn
          (superchat-tape-append "t" 'user "in a" :scope "home")
          (should (equal (superchat-tape-select "SELECT text FROM entry") '(("in a"))))
          ;; Another file at once, then the first one made again from nothing.
          (let ((superchat-directory b))
            (should-not (superchat-tape-select "SELECT text FROM entry")))
          (superchat-tape-close)
          (delete-directory a t)
          (superchat-tape-append "t" 'user "new a" :scope "home")
          (should (equal (superchat-tape-select "SELECT text FROM entry") '(("new a")))))
      (superchat-tape-close)
      (delete-directory dir t))))

(provide 'test-tape)
;;; test-tape.el ends here
