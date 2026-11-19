;;; test-context.el --- tests for superchat-context -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-view)
(require 'superchat-context)

(defun superchat-test-context--write (file text)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun superchat-test-context--project ()
  "A temporary project directory with a .git, as a scope."
  (let ((dir (file-name-as-directory (file-truename (make-temp-file "sc-proj-" t)))))
    (make-directory (expand-file-name ".git" dir))
    dir))

(defun superchat-test-context--at (text)
  "Index of TEXT in the prompt of timeline \"t\", or nil."
  (string-match-p (regexp-quote text) (superchat-context-system "t" "EXTRA-PART")))

(ert-deftest superchat-context-bare ()
  (superchat-test-with-tape
   (superchat-tape-append "t" 'user "hi" :scope "home")
   (let ((system (superchat-context-system "t")))
     (should (string-match-p "Superchat" system))
     (should-not (string-match-p "Rules of" system))
     (should-not (string-match-p "Skills" system))
     (should-not (string-match-p "Notes kept" system))
     (should-not (string-match-p "wrote this" system)))))

(ert-deftest superchat-context-says-the-working-directory ()
  (superchat-test-with-tape
   (let ((dir (superchat-test-context--project))
         (home (expand-file-name "~/")))
     (unwind-protect
         (progn
           (superchat-tape-append "h" 'user "hi" :scope "home")
           (superchat-tape-append "p" 'user "hi" :scope dir)
           (should (string-match-p (regexp-quote (format "The working directory is %s:" home))
                                   (superchat-context-system "h")))
           (should (string-match-p (regexp-quote (format "The working directory is %s:" dir))
                                   (superchat-context-system "p")))
           (should-not (string-match-p "working directory" (superchat-context-system "none"))))
       (delete-directory dir t)))))

(ert-deftest superchat-context-every-part-in-order ()
  (superchat-test-with-tape
   (let ((dir (superchat-test-context--project)))
     (unwind-protect
         (progn
           (superchat-test-context--write (concat dir "AGENTS.md") "RULES-TEXT")
           (superchat-test-context--write
            (concat superchat-directory "skills/pdf/SKILL.md")
            "---\nname: pdf\ndescription: Make PDFs\n---\nSKILL-BODY\n")
           (superchat-tape-append "t" 'user "hi" :scope dir
                                  :data '(:origin (:buffer "a.el" :line 3)))
           (superchat-tape-append "t" 'note "NOTE-TEXT" :data '(:title "n"))
           (let ((parts (mapcar #'superchat-test-context--at
                                (list (format "The working directory is %s:" dir)
                                      "RULES-TEXT" "Make PDFs" "NOTE-TEXT"
                                      "a.el" "EXTRA-PART"))))
             (should (cl-every #'identity parts))
             (should (equal parts (sort (copy-sequence parts) #'<))))
           (should (string-match-p "SKILL.md" (superchat-context-system "t")))
           (should-not (string-match-p "SKILL-BODY" (superchat-context-system "t"))))
       (delete-directory dir t)))))

(ert-deftest superchat-context-home-rules-file ()
  (superchat-test-with-tape
   (superchat-tape-append "t" 'user "hi" :scope "home")
   (should-not (superchat-test-context--at "HOME-RULES"))
   (superchat-test-context--write (concat superchat-directory "AGENTS.md") "HOME-RULES")
   (should (superchat-test-context--at "HOME-RULES"))))

(ert-deftest superchat-context-frontmatter-rules ()
  (superchat-test-with-tape
   (superchat-tape-append "t" 'user "hi" :scope "home")
   (dolist (case '(("a" "---\nname: a\ndescription: Good one\n---\ndescription: in body\n")
                   ("b" "intro\n---\ndescription: not frontmatter\n---\n")
                   ("c" "---\nname: c\n---\ndescription: after the end\n")
                   ("d" "---\nname: d\ndescription: Dash line\n--- \ndescription: late\n---\n")
                   ("e" "---\nname: e\ndescription: never closed\n")))
     (superchat-test-context--write
      (concat superchat-directory "skills/" (car case) "/SKILL.md") (cadr case)))
   (let ((system (superchat-context-system "t")))
     (should (string-match-p "a: Good one" system))
     (should (string-match-p "b: no description" system))
     (should (string-match-p "c: no description" system))
     (should (string-match-p "d: Dash line" system))
     (should (string-match-p "e: no description" system))
     (should-not (string-match-p "in body\\|not frontmatter\\|after the end" system)))))

(ert-deftest superchat-context-notes-limit-and-scope ()
  (superchat-test-with-tape
   (superchat-tape-append "t" 'user "hi" :scope "/p/")
   (superchat-tape-append "o" 'note "OTHER-SCOPE" :scope "/q/")
   (superchat-tape-append "t" 'note (concat "OLD-NOTE" (make-string 1500 ?x)))
   (superchat-tape-append "t" 'note (concat "NEW-NOTE" (make-string 1500 ?y)))
   (let* ((system (superchat-context-system "t"))
          (notes (substring system (string-match "Notes kept" system))))
     (should (<= (length notes) (+ 2000 (length "Notes kept from earlier conversations:\n"))))
     (should (string-match-p "NEW-NOTE" notes))
     (should-not (string-match-p "OLD-NOTE" notes))
     (should-not (string-match-p "OTHER-SCOPE" system)))
   (superchat-tape-append "h" 'note "HOME-NOTE" :scope "home")
   (should (string-match-p "HOME-NOTE" (superchat-context-system "t")))))

(ert-deftest superchat-context-origin-is-the-latest-user-entry ()
  (superchat-test-with-tape
   (superchat-tape-append "t" 'user "one" :scope "home" :data '(:origin (:buffer "old.el")))
   (superchat-tape-append "t" 'user "two")
   (should-not (string-match-p "old.el" (superchat-context-system "t")))
   (superchat-tape-append "t" 'user "three" :data '(:origin (:buffer "new.el" :mode "emacs-lisp-mode")))
   (should (string-match-p "new.el.*emacs-lisp-mode" (superchat-context-system "t")))))

(ert-deftest superchat-context-a-timeline-with-no-entry-has-no-rules-and-no-notes ()
  (superchat-test-with-tape
   (let ((default-directory (file-name-as-directory (make-temp-file "sc-ctx-" t))))
     (unwind-protect
         (progn
           (superchat-test-context--write (concat default-directory "AGENTS.md") "HERE-RULES")
           (superchat-test-context--write (concat superchat-directory "AGENTS.md") "HOME-RULES")
           (superchat-tape-append "h" 'note "HOME-NOTE" :scope "home")
           (superchat-tape-append "p" 'note "PROJECT-NOTE" :scope "/p/")
           (let ((system (superchat-context-system "empty" "EXTRA-PART"))
                 (case-fold-search nil))
             (should (string-match-p "Superchat" system))
             (should (string-match-p "EXTRA-PART" system))
             (should-not (string-match-p "RULES\\|Rules of\\|NOTE\\|Notes kept" system))))
       (delete-directory default-directory t)))))

(ert-deftest superchat-context-the-base-instructions-are-at-most-40-lines ()
  (superchat-test-with-tape
   (should (<= (length (split-string (superchat-context-system "empty") "\n")) 40))))

(ert-deftest superchat-context-the-base-instructions-ask-for-calls-together ()
  (superchat-test-with-tape
   (should (string-search "Calls that do not wait on each other go in one response"
                          (superchat-context-system "empty")))))

(provide 'test-context)
;;; test-context.el ends here
