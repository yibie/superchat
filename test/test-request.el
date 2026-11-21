;;; test-request.el --- tests for superchat-request -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-request)

(defun superchat-test-request--e (id kind text &optional data)
  (list :id id :kind kind :text text :data data))

(ert-deftest superchat-request-messages-keep-a-call-with-its-result ()
  (let ((entries (list (superchat-test-request--e 1 'user "go")
                       (superchat-test-request--e 2 'assistant ""
                                                  '(:calls ((:id "c1" :name "t" :args nil))))
                       (superchat-test-request--e 3 'timer "later" '(:at 1.0))
                       (superchat-test-request--e 4 'tool "done" '(:call-id "c1" :name "t"))
                       (superchat-test-request--e 5 'event "x" '(:type "level")))))
    (should (equal (mapcar (lambda (m) (plist-get m :role)) (superchat-request-messages entries))
                   '(user assistant tool)))
    (should-not (superchat-request-message (nth 2 entries)))
    (should (= (superchat-request-size (superchat-request-messages entries))
               (+ 2 0 (length "nil") (length "#4 done"))))))

(ert-deftest superchat-request-budget-follows-the-option-else-the-window ()
  (let ((superchat-context-chars 500))
    (should (= (superchat-request-budget) 500)))
  (let ((superchat-context-chars nil))
    (cl-letf (((symbol-function 'superchat-llm-context-window) (lambda (&optional _) 8000)))
      (should (= (superchat-request-budget "m") 4000)))
    (cl-letf (((symbol-function 'superchat-llm-context-window) (lambda (&optional _) nil)))
      (should (= (superchat-request-budget "m") 120000)))))

(ert-deftest superchat-request-fit-leaves-out-the-oldest-and-keeps-the-run ()
  (let* ((entries (list (superchat-test-request--e 1 'user (make-string 500 ?a))
                        (superchat-test-request--e 2 'assistant (make-string 100 ?b))
                        (superchat-test-request--e 3 'user "now")))
         (fit (superchat-request-fit entries 3 50)))
    ;; The latest assistant entry is never left out, so only the first goes.
    (should (string-match-p "\\`\\[1 earlier entries" (plist-get (car fit) :text)))
    (should (equal (mapcar (lambda (m) (plist-get m :role)) fit) '(user assistant user)))
    (should (equal (plist-get (car (last fit)) :text) "now"))
    (should (= (superchat-request-kept-size entries 3) 103))))

(ert-deftest superchat-request-digest-counts-the-calls ()
  (let ((entries
         (list (list :id 1 :kind 'assistant
                     :data '(:calls ((:id "a" :name "edit" :args (:path "x.el" :old "1" :new "2"))
                                     (:id "b" :name "shell" :args (:command "make")))))
               (list :id 2 :kind 'tool :text "Replaced 1 block in x.el." :data '(:call-id "a" :name "edit"))
               (list :id 3 :kind 'tool :text "make: *** Error 2\nmore" :data '(:call-id "b" :name "shell" :error t))
               (list :id 4 :kind 'assistant
                     :data '(:calls ((:id "c" :name "edit" :args (:path "x.el" :old "2" :new "3"))
                                     (:id "d" :name "shell" :args (:command "make")))))
               (list :id 5 :kind 'tool :text "Error: OLD occurs 0 times" :data '(:call-id "c" :name "edit" :error t))
               (list :id 6 :kind 'tool :text "make: *** Error 2" :data '(:call-id "d" :name "shell" :error t)))))
    (should (equal (superchat-request-digest entries)
                   (concat "Calls: edit 2 (1 failed), shell 2 (2 failed).\n"
                           "Files changed: x.el 2 times.\n"
                           "Same call made more than once: shell (:command \"make\"), 2 times.\n"
                           "Last failures:\n"
                           "- shell: make: *** Error 2\n"
                           "- edit: Error: OLD occurs 0 times\n"
                           "- shell: make: *** Error 2"))))
  (should (equal (superchat-request-digest nil) "No tool calls.")))

(provide 'test-request)
;;; test-request.el ends here

(ert-deftest superchat-request-sources-keeps-only-this-runs-good-results ()
  (superchat-test-with-tape
    (cl-flet ((add (tape kind text &optional data)
                (plist-get (superchat-tape-append tape kind text :data data :scope "s") :id)))
      (let* ((old (add "t" 'tool "before the run"))
             (user (add "t" 'user "go"))
             (read (add "t" 'tool "the text" '(:call-id "c1" :name "read")))
             (read2 (add "t" 'tool "more text" '(:call-id "c4" :name "read")))
             (bad (add "t" 'tool "Error: no" '(:call-id "c2" :name "eval_elisp" :error t)))
             (other (add "u" 'tool "elsewhere" '(:call-id "c3" :name "read")))
             (answer (add "t" 'assistant "x")))
        (should (equal (superchat-request-sources
                        (format "It says so.\n\nSources: #%d, #%d, #%d, #%d, #%d, #999999\n" read bad other old answer)
                        "t" user)
                       (list "It says so." (list read) 5)))
        ;; With no line, the text is kept as it is.
        (should (equal (superchat-request-sources "Plain.\nSources are many." "t" user)
                       (list "Plain.\nSources are many." nil 0)))
        (should (equal (superchat-request-sources (format "Sources: #%d" read) "t" user)
                       (list "" (list read) 0)))
        ;; Two good ids drop nothing.
        (should (equal (superchat-request-sources (format "Seen.\nSources: #%d, #%d" read read2) "t" user)
                       (list "Seen." (list read read2) 0)))
        ;; Text before the line, and an id twice, drop nothing that is good.
        (should (equal (superchat-request-sources (format "Seen.  \nSources: #%d, #%d" read read) "t" user)
                       (list "Seen." (list read) 0)))))))
