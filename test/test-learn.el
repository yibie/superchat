;;; test-learn.el --- Tests of superchat-learn -*- lexical-binding: t; -*-

(require 'ert)
(require 'helper)
(require 'superchat-learn)

(ert-deftest superchat-learn-check-keeps-only-code-that-ran ()
  (let* ((draft "---\nname: pkg\ndescription: Use for pkg.\n---\n## Get\nGets one.\n```elisp\n(pkg-get 1)\n```\n## Made up\nNot run.\n```elisp\n(pkg-invented)\n```\n")
         (checked (superchat-learn--check draft '("(pkg-get 1)") "2.3")))
    (should (= (cdr checked) 1))
    (should (string-match-p "^name: pkg\nversion: 2.3\n" (car checked)))
    (should (string-match-p "```elisp\n(pkg-get 1)\n```" (car checked)))
    (should-not (string-match-p "pkg-invented" (car checked)))
    (should (string-match-p "not code that ran" (car checked)))))

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
                                (insert-file-contents (expand-file-name "skills/pkg-/SKILL.md"
                                                                        superchat-directory))
                                (buffer-string)))))))

(ert-deftest superchat-learn-with-nothing-cited-says-so ()
  (superchat-test-with-tape
    (should-error (superchat-learn "nothing-") :type 'user-error)))

;;; test-learn.el ends here
