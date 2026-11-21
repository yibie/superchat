;;; test-learn.el --- Tests of superchat-learn -*- lexical-binding: t; -*-

(require 'ert)
(require 'helper)
(require 'superchat-learn)

(ert-deftest superchat-learn-check-keeps-only-code-that-ran ()
  (let* ((draft "---\nname: pkg\ndescription: Use for pkg.\n---\n## Get\nGets one.\n```elisp\n(pkg-get 1)\n```\n## Made up\nNot run.\n```elisp\n(pkg-invented)\n```\n")
         (group (list :key (superchat-learn--key "(pkg-get 1)") :ids '(5 9)))
         (checked (superchat-learn--check draft (list group) "2.3")))
    (should (= (nth 1 checked) 1))
    (should (equal (nth 2 checked) (list group)))
    (should (string-match-p "^name: pkg\nversion: 2.3\n" (car checked)))
    (should (string-match-p "```elisp\n(pkg-get 1)\n```\n## Made up" (car checked)))
    (should-not (string-match-p "pkg-invented" (car checked)))
    (should (string-match-p "not code that ran" (car checked)))))

(defun superchat-test-learn-chat (chat-id)
  "A function whose argument is named CHAT-ID."
  chat-id)

(ert-deftest superchat-learn-a-literal-is-classed-by-where-it-came-from ()
  (let ((ctx (cons "What day ends February 2027? Load emacs-lisp please."
                   (list '(:id 7 :text "chats: 4815162342 Work") '(:id 8 :text "also emacs-lisp")))))
    (should (eq (superchat-learn--origin 2027 ctx) 'input))
    (should (equal (superchat-learn--origin 4815162342 ctx) '(copied . 7)))
    (should (eq (superchat-learn--origin "emacs-lisp" ctx) 'suspected))
    (should (eq (superchat-learn--origin "/tmp/verify.el" ctx) 'unknown))
    (should (eq (superchat-learn--origin 2 ctx) 'short))
    ;; A number is a whole token: 202 is not in 2027.
    (should (eq (superchat-learn--origin 202 ctx) 'unknown))))

(ert-deftest superchat-learn-a-template-holes-inputs-and-copies-only ()
  (require 'calendar)
  (let ((ctx (cons "What day ends February 2027?" (list '(:id 7 :text "chats: 4815162342 Work")))))
    (let ((tpl (superchat-learn--template "(calendar-last-day-of-month 2 2027)" ctx)))
      (should (equal (plist-get tpl :text) "(calendar-last-day-of-month 2 YEAR)"))
      (should (eq (plist-get (car (plist-get tpl :holes)) :class) 'input)))
    (let ((tpl (superchat-learn--template "(superchat-test-learn-chat 4815162342)" ctx)))
      (should (equal (plist-get tpl :text) "(superchat-test-learn-chat CHAT-ID)"))
      (should (equal (plist-get (car (plist-get tpl :holes)) :class) '(copied . 7))))
    (should-not (superchat-learn--producer 999999))
    ;; A bound value is named by the argument it reaches.
    (should (equal (plist-get (superchat-learn--template
                               "(let ((y 2027)) (calendar-last-day-of-month 2 y))" ctx)
                              :text)
                   "(let ((y YEAR)) (calendar-last-day-of-month 2 y))"))
    ;; A value that came from nowhere stays as written.
    (let ((tpl (superchat-learn--template "(load \"/tmp/verify.el\" nil t)" ctx)))
      (should (equal (plist-get tpl :text) "(load \"/tmp/verify.el\" nil t)"))
      (should-not (plist-get tpl :holes))
      (should (equal (plist-get tpl :unknown) '("/tmp/verify.el"))))
    ;; A template that does not give the code back is not kept: here the code
    ;; already holds a symbol named like the hole.
    (let ((tpl (superchat-learn--template "(calendar-last-day-of-month YEAR 2027)" ctx)))
      (should-not (plist-get tpl :holes))
      (should (equal (plist-get tpl :text) "(calendar-last-day-of-month YEAR 2027)")))))

(ert-deftest superchat-learn-compares-with-locals-renamed ()
  (should (equal (superchat-learn--key "(let ((d 1)) (f d))") (superchat-learn--key "(let ((day 1))\n  (f day))")))
  (should-not (equal (superchat-learn--key "(f 1)") (superchat-learn--key "(f 2)"))))

(ert-deftest superchat-learn-records-the-persons-decision-on-a-hole ()
  (let* ((ctx (cons "February 2027" nil))
         (tpl (superchat-learn--template "(calendar-last-day-of-month 2 2027)" ctx))
         (holes (plist-get tpl :holes)))
    (should (equal (superchat-learn--decide (superchat-learn--forms "(calendar-last-day-of-month 2 YEAR)")
                                            (plist-get tpl :forms) holes)
                   (list (cons (car holes) 'hole))))
    (should (equal (superchat-learn--decide (superchat-learn--forms "(calendar-last-day-of-month 2 2027)")
                                            (plist-get tpl :forms) holes)
                   (list (cons (car holes) 'keep))))
    (should (eq (superchat-learn--decide (superchat-learn--forms "(calendar-last-day-of-month 3 YEAR)")
                                         (plist-get tpl :forms) holes)
                :fail))
    ;; A kept value is used in later drafts in place of the origin.
    (let ((superchat-learn--roles '(((calendar-last-day-of-month . 1) . keep))))
      (should-not (plist-get (superchat-learn--template "(calendar-last-day-of-month 2 2027)" ctx) :holes)))))

(ert-deftest superchat-learn-drafts-from-cited-code-and-saves-on-request ()
  (superchat-test-with-tape
    (let ((superchat-directory (file-name-as-directory superchat-test--made)))
      (cl-flet ((add (kind text &optional data refs)
                  (plist-get (superchat-tape-append "t" kind text :data data :refs refs :scope "s") :id)))
        (add 'user "go")
        (add 'assistant "" (list :calls (list (list :id "c1" :name "eval_elisp"
                                                    :args (list :code "(pkg-get 1)")))))
        (let ((good (add 'tool "42" '(:call-id "c1" :name "eval_elisp"))))
          (add 'assistant "it is 42" nil (list good))))
      (superchat-test-script
          (list '(:text "---\nname: pkg\ndescription: Use for pkg.\n---\n## Get\nGets one.\n```elisp\n(pkg-get 1)\n```\n"))
        (superchat-learn "pkg-")
        (superchat-test-wait (lambda () (get-buffer "*superchat: learn pkg-*")))
        ;; The request named the form and what it returned.
        (should (string-match-p "Form:\n(pkg-get 1)\nIt returned: 42"
                                (plist-get (car (last (plist-get (car superchat-test-requests) :messages)))
                                           :text))))
      (with-current-buffer "*superchat: learn pkg-*"
        (should (string-match-p "C-c C-c save" (format "%s" header-line-format)))
        (superchat-learn-save))
      (should (string-match-p "(pkg-get 1)"
                              (with-temp-buffer
                                (insert-file-contents (expand-file-name "skills/pkg/SKILL.md"
                                                                        superchat-directory))
                                (buffer-string)))))))

(ert-deftest superchat-learn-shows-no-reply-that-is-not-a-draft ()
  (superchat-test-with-tape
    (cl-flet ((add (kind text &optional data refs)
                (plist-get (superchat-tape-append "t" kind text :data data :refs refs :scope "s") :id)))
      (add 'user "go")
      (add 'assistant "" (list :calls (list (list :id "c1" :name "eval_elisp"
                                                  :args (list :code "(pkg-get 1)")))))
      (add 'assistant "it is 42" nil (list (add 'tool "42" '(:call-id "c1" :name "eval_elisp")))))
    (let (said)
      (cl-letf (((symbol-function 'message) (lambda (f &rest a) (setq said (apply #'format f a)))))
        (superchat-test-script (list '(:text "I'll first find out what it returns."))
          (superchat-learn "pkg-")
          (superchat-test-wait (lambda () (and said (string-match-p "no draft" said))))))
      (should-not (get-buffer "*superchat: learn pkg-*")))))

(ert-deftest superchat-learn-with-nothing-cited-says-so ()
  (superchat-test-with-tape
    (should-error (superchat-learn "nothing-") :type 'user-error)))

;;; test-learn.el ends here

(ert-deftest superchat-learn-version-of-a-library-and-of-emacs-own ()
  (should (equal (superchat-learn--version "calendar-") (concat "Emacs " emacs-version)))
  (should-not (superchat-learn--version "superchat-no-such-library-")))

(ert-deftest superchat-learn-adds-only-new-forms-and-remembers-dropped-ones ()
  (superchat-test-with-tape
    (let* ((superchat-directory (file-name-as-directory superchat-test--made))
           (file (expand-file-name "skills/pkg/SKILL.md" superchat-directory)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "---\nname: pkg\nversion: 1.0\ndescription: Use for pkg.\n---\n## Get\nMine.\n```elisp\n(pkg-get 1)\n```\n"))
      (cl-flet ((cite (id code result)
                  (superchat-tape-append "t" 'user "go" :scope "s")
                  (superchat-tape-append "t" 'assistant ""
                                         :data (list :calls (list (list :id id :name "eval_elisp"
                                                                        :args (list :code code)))))
                  (let ((tool (plist-get (superchat-tape-append "t" 'tool result
                                                                :data (list :call-id id :name "eval_elisp"))
                                         :id)))
                    (superchat-tape-append "t" 'assistant "ok" :refs (list tool)))))
        (cite "c1" "(pkg-get 1)" "42")
        (cite "c2" "(pkg-put 2)" "t")
        (cite "c3" "(pkg-del 3)" "nil"))
      (cl-letf (((symbol-function 'superchat-learn--version) (lambda (_) "2.0")))
        (superchat-test-script
            (list '(:text "## Put\nPuts.\n```elisp\n(pkg-put 2)\n```\n## Del\nDeletes.\n```elisp\n(pkg-del 3)\n```\n"))
          (superchat-learn "pkg-")
          (superchat-test-wait (lambda () (get-buffer "*superchat: learn pkg-*")))
          ;; Only the forms not in the skill were asked for.
          (let ((asked (plist-get (car (last (plist-get (car superchat-test-requests) :messages))) :text)))
            (should (string-match-p "(pkg-put 2)" asked))
            (should-not (string-match-p "(pkg-get 1)" asked)))))
      (with-current-buffer "*superchat: learn pkg-*"
        (should (string-match-p "add to" (format "%s" header-line-format)))
        (should (string-match-p "1\\.0 → 2\\.0" (format "%s" header-line-format)))
        ;; The person deletes the Del section, then saves.
        (goto-char (point-min))
        (re-search-forward "^## Del")
        (delete-region (line-beginning-position) (point-max))
        (superchat-learn-save))
      (let ((saved (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (string-match-p "## Get\nMine\\." saved))
        (should (string-match-p "(pkg-put 2)" saved))
        (should-not (string-match-p "pkg-del" saved))
        (should (string-match-p "^version: 2\\.0$" saved)))
      ;; A second /learn finds nothing new: one form is in, one was dropped.
      (let (said)
        (cl-letf (((symbol-function 'message) (lambda (f &rest a) (setq said (apply #'format f a)))))
          (superchat-learn "pkg-"))
        (should (string-match-p "No new form" said))))))

(ert-deftest superchat-learn-drafts-a-template-and-keeps-its-evidence ()
  (require 'calendar)
  (superchat-test-with-tape
    (let* ((superchat-directory (file-name-as-directory superchat-test--made))
           (dir (expand-file-name "skills/calendar/" superchat-directory)))
      (superchat-tape-append "t" 'user "What is the last day of February 2027?" :scope "s")
      (superchat-tape-append "t" 'assistant ""
                             :data (list :calls (list (list :id "c1" :name "eval_elisp"
                                                            :args (list :code "(calendar-last-day-of-month 2 2027)")))))
      (let ((tool (plist-get (superchat-tape-append "t" 'tool "28" :data '(:call-id "c1" :name "eval_elisp")) :id)))
        (superchat-tape-append "t" 'assistant "28" :refs (list tool)))
      (superchat-test-script
          (list '(:text "---\nname: calendar\ndescription: Dates.\n---\n## Last day\nThe last day of a month.\n```elisp\n(calendar-last-day-of-month 2 YEAR)\n```\n"))
        (superchat-learn "calendar-")
        (superchat-test-wait (lambda () (get-buffer "*superchat: learn calendar-*")))
        (should (string-match-p "Holes: YEAR, here 2027, from the request"
                                (plist-get (car (last (plist-get (car superchat-test-requests) :messages))) :text))))
      (with-current-buffer "*superchat: learn calendar-*"
        (should (string-match-p "```\nYEAR: from the request\\." (buffer-string)))
        (should (string-match-p "1 hole" (format "%s" header-line-format)))
        (superchat-learn-save))
      (should (string-match-p "(calendar-last-day-of-month 2 YEAR)"
                              (with-temp-buffer (insert-file-contents (expand-file-name "SKILL.md" dir)) (buffer-string))))
      (should (= (length (cdar (superchat-learn--read-data (expand-file-name "evidence.eld" dir)))) 1))
      (should (equal (superchat-learn--read-data (expand-file-name "roles.eld" dir))
                     '(((calendar-last-day-of-month . 1) . hole))))
      (let ((e (car (superchat-learn--read-data (expand-file-name "interface.eld" dir)))))
        (should (equal (plist-get e :name) "last_day_of_month"))
        (should (eq (plist-get e :class) 'write))
        (should (equal (plist-get e :args) '((:name "year" :type number))))
        (should (string-prefix-p "Last day\nyear: from the request." (plist-get e :doc)))
        (should (= (eval (cl-subst 2028 'year (car (read-from-string (plist-get e :code))))) 29))))))

(ert-deftest superchat-learn-an-interface-entry-is-read-only-when-nothing-changes ()
  (should (eq (superchat-learn--class-of '((calendar-leap-year-p 2028))) 'read))
  (should (eq (superchat-learn--class-of '((let ((x (car '(1)))) (list x)))) 'read))
  (should (eq (superchat-learn--class-of '((progn (require 'calendar) (insert "x")))) 'write))
  (let* ((y (make-symbol "YEAR"))
         (varies (list :name "YEAR" :symbol y :value 2027 :class 'varies))
         (input (list :name "YEAR" :symbol y :value 2027 :class 'input)))
    (should-not (superchat-learn--entry "pkg-" (list :forms (list (list 'pkg-write 'YEAR)))
                                        (list (cons varies 'hole)) nil nil))
    (should (superchat-learn--entry "pkg-" (list :forms (list (list 'pkg-write 'YEAR)))
                                    (list (cons input 'hole)) nil nil))
    (should (equal (plist-get (superchat-learn--entry "pkg-" (list :forms (list (list 'pkg-write 'YEAR)))
                                                      (list (cons input 'hole)) nil '("write"))
                              :name)
                   "write_2"))))

(ert-deftest superchat-learn-runs-of-one-shape-share-a-template ()
  (require 'calendar)
  (let* ((ctx (lambda (req) (cons req nil)))
         (run (lambda (code req)
                (append (list :code (superchat-learn--forms code))
                        (superchat-learn--template code (funcall ctx req)))))
         (a (funcall run "(calendar-last-day-of-month 2 2028)" "February 2028"))
         (b (funcall run "(calendar-last-day-of-month 4 2025)" "April 2025"))
         (general (superchat-learn--generalize (list a b))))
    (should (equal (superchat-learn--shape (plist-get a :forms)) (superchat-learn--shape (plist-get b :forms))))
    (should (equal (plist-get general :forms) '((calendar-last-day-of-month MONTH YEAR))))
    (should (equal (mapcar (lambda (h) (list (plist-get h :name) (plist-get h :class))) (plist-get general :holes))
                   '(("MONTH" varies) ("YEAR" input)))))
  ;; A hole nested in a part equal in every run still gets each run's value.
  (let* ((run (lambda (code req) (append (list :code (superchat-learn--forms code))
                                         (superchat-learn--template code (cons req nil)))))
         (general (superchat-learn--generalize
                   (list (funcall run "(list (car (directory-files \"/tmp/reports\")) \"org\")" "/tmp/reports")
                         (funcall run "(list (car (directory-files \"/tmp/logs\")) \"md\")" "/tmp/logs")))))
    (should (equal (plist-get general :forms) '((list (car (directory-files DIRECTORY)) OBJECTS)))))
  ;; Different functions are different shapes.
  (should-not (equal (superchat-learn--shape (superchat-learn--forms "(number-to-string 42)"))
                     (superchat-learn--shape (superchat-learn--forms "(string-to-number \"42\")")))))
