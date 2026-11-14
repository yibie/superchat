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
      (should (string-match-p (concat "x\\{400\\}\\.\\.\\.\\'") text))
      (should-not (string-match-p "x\\{401\\}" text))
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

(provide 'test-view)
;;; test-view.el ends here
