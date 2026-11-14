;;; test-recall.el --- tests for superchat-recall -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-permission)
(require 'superchat-recall)

(defmacro superchat-test-recall--with (&rest body)
  "Run BODY on timeline \"t\" (scope \"/p/\", level auto)."
  (declare (indent 0))
  `(superchat-test-with-tape
     (superchat-tape-append "t" 'user "the ferret plan" :scope "/p/")
     (superchat-permission-set "t" 'auto)
     ,@body))

(defun superchat-test-recall--call (name &rest args)
  "The tool entry of NAME called with ARGS."
  (superchat-tool-call "t" (list :id "c" :name name :args args)))

(defun superchat-test-recall--ok (name &rest args)
  "Text of a call that must succeed."
  (let ((e (apply #'superchat-test-recall--call name args)))
    (should-not (plist-get (plist-get e :data) :error))
    (plist-get e :text)))

(defun superchat-test-recall--err (name &rest args)
  "Text of a call that must fail."
  (let ((e (apply #'superchat-test-recall--call name args)))
    (should (eq t (plist-get (plist-get e :data) :error)))
    (plist-get e :text)))

(ert-deftest superchat-test-recall-views ()
  (superchat-test-recall--with
    (superchat-tape-append "t" 'anchor "sum" :data '(:name "plans" :summary "sum" :next ""))
    (superchat-tape-append "t" 'assistant "ferret ok")
    (superchat-tape-append "other" 'user "elsewhere ferret" :scope "/q/")
    (let ((note (superchat-tape-append "t" 'note "kept" :data '(:title "k") :refs '(1))))
      (should (string-match-p "ferret ok" (superchat-test-recall--ok "recall" :view "conversation")))
      (should (string-match-p "ferret ok" (superchat-test-recall--ok "recall" :view "topic" :name "plans")))
      (let ((hits (superchat-test-recall--ok "recall" :view "search" :query "ferret")))
        (should (string-match-p "the ferret plan" hits))
        (should-not (string-match-p "elsewhere" hits)))
      (should (string-match-p "sum" (superchat-test-recall--ok "recall" :view "timelines" :limit "1")))
      (should (string-match-p "kept" (superchat-test-recall--ok "recall" :view "notes")))
      (should (string-match-p "the ferret plan"
                              (superchat-test-recall--ok "recall" :view "sources"
                                                         :id (format "%d" (plist-get note :id))))))))

(ert-deftest superchat-test-recall-limit-and-unknown ()
  (superchat-test-recall--with
    (dotimes (_ 30) (superchat-tape-append "t" 'assistant (make-string 400 ?x)))
    (let ((text (superchat-test-recall--ok "recall" :view "conversation")))
      (should (<= (length text) 6000))
      (should (string-match-p "earlier entries left out" text)))
    (should (string-match-p "Unknown view" (superchat-test-recall--err "recall" :view "nope")))))

(ert-deftest superchat-test-recall-description-lists-new-view ()
  (unwind-protect
      (progn
        (superchat-view-define 'zebra-test #'ignore ":x. A view defined after loading.")
        (let ((spec (car (superchat-tool-specs '("recall")))))
          (should (string-match-p "zebra-test" (plist-get spec :description)))))
    (superchat-view-remove 'zebra-test)))

(ert-deftest superchat-test-recall-tape-query ()
  (superchat-test-recall--with
    (should (equal "[1,\"user\"]"
                   (superchat-test-recall--ok "tape_query" :sql "SELECT id, kind FROM entry WHERE kind = 'user'")))
    (dotimes (_ 250) (superchat-tape-append "t" 'event "e"))
    (let ((lines (split-string (superchat-test-recall--ok "tape_query" :sql "SELECT id FROM entry") "\n")))
      (should (= 201 (length lines)))
      (should (string-match-p "more rows left out" (car (last lines)))))
    (let* ((start (float-time))
           (text (superchat-test-recall--ok
                  "tape_query" :sql "WITH RECURSIVE n(i) AS (SELECT 1 UNION ALL SELECT i+1 FROM n WHERE i < 100000) SELECT i FROM n;
"))
           (lines (split-string text "\n")))
      (should (< (- (float-time) start) 2))
      (should (= 201 (length lines)))
      (should (string-match-p "more rows left out" (car (last lines)))))
    (should (equal "[1]" (superchat-test-recall--ok "tape_query" :sql "SELECT 1 -- c")))
    (should (equal "[2]" (superchat-test-recall--ok "tape_query" :sql "-- first\nSELECT 2")))
    (superchat-test-recall--err "tape_query" :sql "DELETE FROM entry")
    (superchat-test-recall--err "tape_query" :sql "SELECT * FROM nothing_here")
    (should (= 251 (caar (superchat-tape-select "SELECT COUNT(*) FROM entry WHERE kind = 'event'"))))))

(ert-deftest superchat-test-recall-note ()
  (superchat-test-recall--with
    (let ((assistant (superchat-tape-append "t" 'assistant "the answer")))
      (should (string-match-p "Note saved as #"
                              (superchat-test-recall--ok
                               "note" :title "T" :text "ferrets win"
                               :sources (format "1, %d" (plist-get assistant :id)))))
      (let ((n (car (superchat-tape-entries :kinds '(note)))))
        (should (equal "T" (plist-get (plist-get n :data) :title)))
        (should (equal (list 1 (plist-get assistant :id)) (plist-get n :refs)))
        (should (equal "t" (plist-get n :tape)))
        (should (string-match-p "the answer"
                                (superchat-test-recall--ok "recall" :view "sources" :id (plist-get n :id)))))
      (should (string-match-p "ferrets win" (superchat-test-recall--ok "recall" :view "notes")))
      (should (string-match-p "999" (superchat-test-recall--err "note" :title "x" :text "y" :sources "1,999")))
      (should (= 1 (length (superchat-tape-entries :kinds '(note))))))))

(provide 'test-recall)
;;; test-recall.el ends here
