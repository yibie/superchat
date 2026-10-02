;;; recall-cjk-regression.el --- recall matches Chinese sentences -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 2.1 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-2-rules-and-recall.md):
;; a run of CJK characters in the query is cut into overlapping
;; three-character pieces, which is what the trigram index can match.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/recall-cjk-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^recall-cjk/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(when (getenv "SMOKE_REAL")
  (error "Recall CJK regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defun recall-cjk--pieces (query)
  "The quoted pieces of QUERY's MATCH string, as a list of strings."
  (let ((match (superchat-db--tape-search-match-query query)))
    (and match
         (mapcar (lambda (piece) (string-trim piece "\"" "\""))
                 (split-string match " OR ")))))

(ert-deftest recall-cjk/a-chinese-run-is-cut-into-overlapping-threes ()
  (should (equal '("今天的" "天的日" "的日记")
                 (recall-cjk--pieces "今天的日记"))))

(ert-deftest recall-cjk/a-chinese-run-of-three-is-one-piece ()
  (should (equal '("日记本") (recall-cjk--pieces "日记本"))))

(ert-deftest recall-cjk/a-chinese-run-shorter-than-three-is-dropped ()
  (should-not (recall-cjk--pieces "日记"))
  (should (equal '("hello") (recall-cjk--pieces "日记 hello"))))

(ert-deftest recall-cjk/latin-words-are-kept-whole ()
  (should (equal '("scratch" "org") (recall-cjk--pieces "scratch.org")))
  (should-not (recall-cjk--pieces "a bc")))

(ert-deftest recall-cjk/mixed-text-gives-both-kinds ()
  (should (equal '("scratch" "末尾加" "尾加一" "加一篇")
                 (recall-cjk--pieces "scratch 末尾加一篇"))))

(ert-deftest recall-cjk/pieces-are-not-repeated ()
  (should (equal '("哈哈哈") (recall-cjk--pieces "哈哈哈哈哈"))))

(ert-deftest recall-cjk/a-long-query-is-capped ()
  (let ((pieces (recall-cjk--pieces (make-string 300 ?\x4e00))))
    (should (= 1 (length pieces))))
  (let* ((text (mapconcat (lambda (i) (string (+ #x4e00 i))) (number-sequence 0 199) ""))
         (pieces (recall-cjk--pieces text)))
    (should (= 48 (length pieces)))))

(ert-deftest recall-cjk/query-syntax-never-reaches-the-match-string ()
  (should (equal "\"say\" OR \"hello\""
                 (superchat-db--tape-search-match-query "say\"hello")))
  (should (equal "\"NEAR\" OR \"foo\""
                 (superchat-db--tape-search-match-query "NEAR(foo) OR *"))))

(ert-deftest recall-cjk/a-stored-note-is-found-by-a-chinese-request ()
  "The case from the issue: the note and the request share only 的日记."
  (smoke--with-fixture nil
    (superchat-tape-append-material
     "s-note" "user" "我写的日记，标题一律用英文，正文用中文。")
    (let ((rows (superchat-db-tape-search
                 "在 scratch.org 末尾加一篇今天的日记，标题你来起。" nil 5)))
      (should rows)
      (should (cl-some (lambda (row) (string-match-p "标题一律用英文" (format "%S" row)))
                       rows)))))

(provide 'recall-cjk-regression)
;;; recall-cjk-regression.el ends here
