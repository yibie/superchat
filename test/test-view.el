;;; test-view.el --- tests for superchat-view -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-view)

(defun superchat-test-view--add (tape kind text &rest keys)
  "Append and return the id."
  (plist-get (apply #'superchat-tape-append tape kind text :scope "/p/" keys) :id))

(defun superchat-test-view--texts (entries)
  (mapcar (lambda (e) (plist-get e :text)) entries))

(defun superchat-test-view--anchor (tape text name &rest keys)
  (apply #'superchat-test-view--add tape 'anchor text :data (list :name name) keys))

(ert-deftest superchat-view-registry ()
  (should-error (superchat-view 'no-such-view))
  (superchat-view-define 'mine (lambda (args) (list (plist-get args :x))) "doc")
  (unwind-protect
      (progn (should (equal (superchat-view 'mine :x 7) '(7)))
             (should (equal (cdr (assq 'mine (superchat-view-names))) "doc"))
             (superchat-view-define 'mine (lambda (_) '(8)) "again")
             (should (equal (superchat-view 'mine) '(8))))
    (superchat-view-remove 'mine))
  (should-error (superchat-view 'mine))
  (should-not (assq 'mine (superchat-view-names)))
  (dolist (n '(conversation topic search notes timelines sources))
    (should (assq n (superchat-view-names)))))

(ert-deftest superchat-view-conversation-without-anchor ()
  (superchat-test-with-tape
    (superchat-test-view--add "a" 'user "one")
    (superchat-test-view--add "b" 'user "other")
    (superchat-test-view--add "a" 'assistant "two")
    (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "a"))
                   '("one" "two")))))

(ert-deftest superchat-view-conversation-starts-at-latest-anchor ()
  (superchat-test-with-tape
    (superchat-test-view--add "a" 'user "u1")
    (superchat-test-view--anchor "a" "s1" "x")
    (superchat-test-view--add "a" 'user "u2")
    (superchat-test-view--anchor "a" "s2" "x")
    (superchat-test-view--add "a" 'user "u3")
    (superchat-test-view--add "b" 'user "elsewhere")
    (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "a"))
                   '("s2" "u3")))))

(ert-deftest superchat-view-conversation-of-a-fork ()
  (superchat-test-with-tape
    (let* ((u1 (superchat-test-view--add "a" 'user "u1"))
           (_ (superchat-test-view--add "a" 'user "skipped"))
           (u3 (superchat-test-view--add "a" 'user "u3")))
      (superchat-test-view--anchor "f" "" "fork" :refs (list u3 u1))
      (superchat-test-view--add "f" 'user "new")
      (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "f"))
                     '("u1" "u3" "" "new")))
      (superchat-test-view--anchor "f" "summary" "handoff")
      (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "f"))
                     '("summary"))))))

(ert-deftest superchat-view-topic-across-timelines ()
  (superchat-test-with-tape
    (superchat-test-view--anchor "a" "A1" "t")
    (superchat-test-view--add "a" 'user "a-in")
    (superchat-test-view--anchor "a" "A2" "other")
    (superchat-test-view--add "a" 'user "a-out")
    (superchat-test-view--add "b" 'user "b-before")
    (superchat-test-view--anchor "b" "B1" "t")
    (superchat-test-view--add "b" 'user "b-in")
    (should (equal (superchat-test-view--texts (superchat-view 'topic :name "t"))
                   '("A1" "a-in" "B1" "b-in")))
    (should-not (superchat-view 'topic :name "t" :scope "/q/"))
    (should-not (superchat-view 'topic :name "none"))))

(ert-deftest superchat-view-search-neighbours ()
  (superchat-test-with-tape
    (dolist (text '("zero" "needle one" "two" "three" "four" "needle five" "six"))
      (superchat-test-view--add "a" 'user text))
    (superchat-test-view--add "b" 'user "b needle")
    (let ((texts (superchat-test-view--texts (superchat-view 'search :query "needle"))))
      (should (equal texts '("zero" "needle one" "two" "four" "needle five" "six"
                             "b needle"))))
    (should (equal (superchat-test-view--texts
                    (superchat-view 'search :query "needle" :around 0 :scope "/p/"))
                   '("needle one" "needle five" "b needle")))
    (should (equal (superchat-test-view--texts
                    (superchat-view 'search :query "needle one" :around 2))
                   '("zero" "needle one" "two" "three")))
    (should (= 1 (length (superchat-view 'search :query "needle" :around 0 :limit 1))))
    (should (equal (superchat-test-view--texts
                    (superchat-view 'search :query "needle" :around 0 :kinds '(note)))
                   nil))))

(ert-deftest superchat-view-notes ()
  (superchat-test-with-tape
    (superchat-tape-append "a" 'note "n1 apple" :scope "/p/")
    (superchat-tape-append "b" 'note "n2 home" :scope "home")
    (superchat-tape-append "c" 'note "n3 other" :scope "/q/")
    (superchat-tape-append "a" 'user "not a note apple" :scope "/p/")
    (superchat-tape-append "a" 'note "n4 apple" :scope "/p/")
    (should (equal (superchat-test-view--texts (superchat-view 'notes))
                   '("n4 apple" "n3 other" "n2 home" "n1 apple")))
    (should (equal (superchat-test-view--texts (superchat-view 'notes :scope "/p/"))
                   '("n4 apple" "n2 home" "n1 apple")))
    (should (equal (superchat-test-view--texts (superchat-view 'notes :scope "/p/" :limit 2))
                   '("n4 apple" "n2 home")))
    (should (equal (superchat-test-view--texts
                    (superchat-view 'notes :scope "/p/" :query "apple"))
                   '("n4 apple" "n1 apple")))
    (should (equal (superchat-test-view--texts (superchat-view 'notes :query "home"))
                   '("n2 home")))))

(ert-deftest superchat-view-notes-query-is-the-newest-matches ()
  (superchat-test-with-tape
    ;; The older notes match best; the limit still takes the newest.
    (dotimes (i 3) (superchat-tape-append (format "o%d" i) 'note "apple apple" :scope "/p/"))
    (superchat-tape-append "h" 'note "an apple, said at home, in a longer note" :scope "home")
    (superchat-tape-append "n" 'note "the newest apple note of all, the longest one" :scope "/p/")
    (should (equal (superchat-test-view--texts
                    (superchat-view 'notes :scope "/p/" :query "apple" :limit 2))
                   '("the newest apple note of all, the longest one"
                     "an apple, said at home, in a longer note")))
    (should (equal (length (superchat-view 'notes :query "apple" :limit 4)) 4))))

(ert-deftest superchat-view-timelines ()
  (superchat-test-with-tape
    (superchat-test-view--add "a" 'event "e")
    (superchat-test-view--add "a" 'user "a-first")
    (superchat-test-view--add "b" 'user "b-first")
    (superchat-test-view--anchor "b" "b-anchor" "x")
    (superchat-test-view--add "b" 'user "b-later")
    (superchat-test-view--add "c" 'event "no user")
    (superchat-test-view--add "a" 'assistant "a-last")
    (superchat-tape-append "d" 'user "d-first" :scope "/q/")
    (should (equal (superchat-test-view--texts (superchat-view 'timelines))
                   '("d-first" "a-first" "b-anchor")))
    (should (equal (superchat-test-view--texts (superchat-view 'timelines :scope "/p/"))
                   '("a-first" "b-anchor")))
    (should (equal (superchat-test-view--texts (superchat-view 'timelines :limit 1))
                   '("d-first")))))

(ert-deftest superchat-view-timelines-leave-out-ephemeral ()
  (superchat-test-with-tape
    (superchat-test-view--add "a" 'user "a-first")
    (superchat-test-view--add "b" 'user "b-first")
    (dotimes (i 3)
      (superchat-test-view--add (format "e%d" i) 'user (format "e-%d" i)
                                :data '(:ephemeral t))
      (superchat-test-view--add (format "e%d" i) 'assistant "answer"))
    (should (equal (superchat-test-view--texts (superchat-view 'timelines))
                   '("b-first" "a-first")))
    (should (equal (superchat-test-view--texts (superchat-view 'timelines :limit 2))
                   '("b-first" "a-first")))))

(ert-deftest superchat-view-timelines-skip-a-timeline-with-no-user-entry ()
  (superchat-test-with-tape
    (superchat-test-view--add "a" 'user "a-first")
    (superchat-test-view--add "n" 'note "only a note" :data '(:title "t"))
    (should (equal (superchat-test-view--texts (superchat-view 'timelines :limit 1))
                   '("a-first")))))

(ert-deftest superchat-view-sources ()
  (superchat-test-with-tape
    (let* ((a (superchat-test-view--add "a" 'user "first"))
           (b (superchat-test-view--add "a" 'user "second"))
           (n (superchat-test-view--add "a" 'note "conclusion" :refs (list b a))))
      (should (equal (superchat-test-view--texts (superchat-view 'sources :id n))
                     '("first" "second")))
      (should-not (superchat-view 'sources :id a))
      (should-not (superchat-view 'sources :id 999)))))

(ert-deftest superchat-view-render-format-and-cut ()
  (superchat-test-with-tape
    (let* ((u (superchat-tape-append "a" 'user "hello" :scope "/p/" :at 0))
           (tool (superchat-tape-append "a" 'tool (make-string 500 ?x) :scope "/p/"))
           (text (superchat-view-render (list u tool))))
      (should (string-match-p (format "\\`#%d user [0-9-]+ [0-9:]+\nhello\n\n#%d tool "
                                      (plist-get u :id) (plist-get tool :id))
                              text))
      (should (string-suffix-p (superchat-view-clip (make-string 500 ?x) 400) text))
      (should (string-match-p (format "#%d tool [0-9-]+ [0-9:]+\n" (plist-get tool :id)) text))
      (should (= (length (cadr (split-string text "\n#[0-9]+ tool [0-9-]+ [0-9:]+\n"))) 400))
      (should (equal (superchat-view-render nil) "")))))

(ert-deftest superchat-view-render-drops-oldest ()
  (superchat-test-with-tape
    (let* ((entries (cl-loop for i from 1 to 5
                             collect (superchat-tape-append
                                      "a" 'user (format "entry-%d %s" i (make-string 90 ?.))
                                      :scope "/p/")))
           (full (superchat-view-render entries))
           (text (superchat-view-render entries 300)))
      (should (string-prefix-p "[" text))
      (should (string-match "\\`\\[\\([0-9]+\\) earlier" text))
      (should (<= (length text) 300))
      (should (string-match-p "entry-5" text))
      (should-not (string-match-p "entry-1 " text))
      (should (equal (superchat-view-render entries (length full)) full)))))

(ert-deftest superchat-view-render-drops-smallest-ids-whatever-the-order ()
  (superchat-test-with-tape
    (let* ((entries (reverse (cl-loop for i from 1 to 5
                                      collect (superchat-tape-append
                                               "a" 'user (format "entry-%d %s" i (make-string 90 ?.))
                                               :scope "/p/"))))
           (text (superchat-view-render entries 300)))
      (should (string-match "\\`\\[\\([0-9]+\\) earlier entries left out\\]\n\n#" text))
      (should (<= (length text) 300))
      (should (string-match-p "entry-5.*\n\\(.\\|\n\\)*entry-4" text))
      (should-not (string-match-p "entry-1 " text)))))

(ert-deftest superchat-view-render-exceeds-the-limit-only-by-the-header ()
  (superchat-test-with-tape
    (let* ((e (superchat-tape-append "a" 'user (make-string 200 ?z) :scope "/p/"))
           (header (format "#%d user %s" (plist-get e :id)
                           (format-time-string "%F %R" (plist-get e :at)))))
      (should (equal (superchat-view-render (list e) (+ (length header) 3)) header))
      (should (equal (superchat-view-render (list e) 3) header)))))

(ert-deftest superchat-view-render-cuts-a-single-block-over-the-limit ()
  (superchat-test-with-tape
    (let* ((big (superchat-tape-append "a" 'user (make-string 5000 ?w) :scope "/p/"))
           (small (superchat-tape-append "a" 'user "tiny" :scope "/p/"))
           (text (superchat-view-render (list big) 1000))
           (two (superchat-view-render (list big small) 1000)))
      (should (<= (length text) 1000))
      (should (string-match-p (format "\\`#%d user " (plist-get big :id)) text))
      (should (string-match-p "characters cut\\]\\'" text))
      (should (string-match-p "w\\{100\\}" text))
      ;; The newest block is the one that stays, whole when it fits.
      (should (<= (length two) 1000))
      (should (string-match-p "earlier entries left out\\]" two))
      (should (string-suffix-p "tiny" two)))))

;; A header is what a block is cited by: a small limit cuts only the text.
(ert-deftest superchat-view-render-small-limit-keeps-the-header-whole ()
  (superchat-test-with-tape
    (let* ((old (superchat-tape-append "a" 'user "older" :scope "/p/"))
           (e (superchat-tape-append "a" 'user (make-string 200 ?z)))
           (text (superchat-view-render (list old e) 90)))
      (should (string-match-p (format "\\`\\[1 earlier entries left out\\]\n\n#%d user [0-9-]+ [0-9:]+\n"
                                      (plist-get e :id))
                              text))
      (should (string-match-p "characters cut\\]\\'" text)))))

(ert-deftest superchat-view-search-overlapping-neighbours-come-once ()
  (superchat-test-with-tape
    (dolist (text '("zero" "needle one" "two" "needle three" "needle four" "five"))
      (superchat-test-view--add "a" 'user text))
    (let ((entries (superchat-view 'search :query "needle" :around 2)))
      (should (equal (superchat-test-view--texts entries)
                     '("zero" "needle one" "two" "needle three" "needle four" "five")))
      (should (equal (mapcar (lambda (e) (plist-get e :id)) entries)
                     (sort (delete-dups (mapcar (lambda (e) (plist-get e :id)) entries)) #'<))))))

(ert-deftest superchat-view-topic-with-a-matching-scope ()
  (superchat-test-with-tape
    (superchat-test-view--anchor "a" "A1" "t")
    (superchat-test-view--add "a" 'user "a-in")
    (superchat-tape-append "q" 'anchor "Q1" :data '(:name "t") :scope "/q/")
    (superchat-tape-append "q" 'user "q-in")
    (should (equal (superchat-test-view--texts (superchat-view 'topic :name "t" :scope "/p/"))
                   '("A1" "a-in")))
    (should (equal (superchat-test-view--texts (superchat-view 'topic :name "t" :scope "/q/"))
                   '("Q1" "q-in")))))

(ert-deftest superchat-view-clip-cuts-with-a-mark ()
  (should (equal (superchat-view-clip "short" 5) "short"))
  (let ((cut (superchat-view-clip (make-string 100 ?a) 40)))
    (should (<= (length cut) 40))
    (should (string-match "\\`\\(a+\\)\\.\\.\\. \\[\\([0-9]+\\) characters cut\\]\\'" cut))
    ;; What is kept and what the mark counts make the whole text.
    (should (= (+ (length (match-string 1 cut)) (string-to-number (match-string 2 cut))) 100)))
  ;; Shorter than the mark: the mark alone, counting every character.
  (should (equal (superchat-view-clip (make-string 100 ?a) 5) "... [100 characters cut]")))

(ert-deftest superchat-view-conversation-of-a-fork-with-missing-refs ()
  (superchat-test-with-tape
    (let ((u1 (superchat-test-view--add "a" 'user "u1")))
      (superchat-test-view--anchor "f" "" "fork" :refs (list 9999 u1))
      (superchat-test-view--add "f" 'user "new")
      (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "f"))
                     '("u1" "" "new"))))))

(ert-deftest superchat-view-conversation-and-search-need-their-argument ()
  (superchat-test-with-tape
    (superchat-test-view--add "a" 'user "one")
    (should-error (superchat-view 'conversation))
    (should-error (superchat-view 'search))
    (should-error (superchat-view 'search :query ""))))

(ert-deftest superchat-view-conversation-of-a-fork-is-its-refs-at-the-latest-anchor-only ()
  (superchat-test-with-tape
    (let ((u1 (superchat-test-view--add "a" 'user "u1")))
      ;; A summary first, then an anchor with refs and no summary: the latest counts.
      (superchat-test-view--anchor "f" "S" "handoff")
      (superchat-test-view--add "f" 'user "after S")
      (superchat-test-view--anchor "f" "" "fork" :refs (list u1))
      (superchat-test-view--add "f" 'user "new")
      (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "f"))
                     '("u1" "" "new")))
      ;; No refs: an empty anchor is only where the conversation starts.
      (superchat-test-view--anchor "g" "" "empty")
      (superchat-test-view--add "g" 'user "x")
      (should (equal (superchat-test-view--texts (superchat-view 'conversation :tape "g"))
                     '("" "x"))))))

(provide 'test-view)
;;; test-view.el ends here
