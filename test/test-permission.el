;;; test-permission.el --- tests for superchat-permission -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-tape)
(require 'superchat-permission)

(ert-deftest superchat-permission-an-answer-is-on-the-tape ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (should-not (superchat-permission-answered "t" "c1"))
    (superchat-permission-answer "t" "c1" 'deny)
    (superchat-permission-answer "t" "c2" 'always)
    (should (eq (superchat-permission-answered "t" "c1") 'deny))
    (should (eq (superchat-permission-answered "t" "c2") 'always))
    (should-not (superchat-permission-answered "u" "c1"))
    (should-error (superchat-permission-answer "t" "c3" 'later))))

(ert-deftest superchat-permission-table ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (dolist (row '((read-only allow deny deny) (ask allow ask ask) (auto allow allow allow)))
      (superchat-permission-set "t" (car row))
      (should (equal (mapcar (lambda (c) (superchat-permission-decide "t" c)) '(read write exec))
                     (cdr row))))))

(ert-deftest superchat-permission-level-is-latest-event-else-default ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (superchat-tape-append "other" 'user "hi" :scope "home")
    (should (eq (superchat-permission-level "t") 'ask))
    (let ((superchat-default-level 'auto))
      (should (eq (superchat-permission-level "t") 'auto))
      (should (eq (superchat-permission-decide "t" 'exec) 'allow)))
    (let ((entry (superchat-permission-set "t" 'read-only)))
      (should (eq (plist-get entry :kind) 'event))
      (should (equal (plist-get (plist-get entry :data) :type) "level")))
    (superchat-tape-append "t" 'event "noise" :data '(:type "error"))
    (should (eq (superchat-permission-level "t") 'read-only))
    (should (eq (superchat-permission-level "other") 'ask))
    (superchat-permission-set "t" 'auto)
    (should (eq (superchat-permission-level "t") 'auto))))

(ert-deftest superchat-permission-set-starts-a-timeline-with-its-scope ()
  (superchat-test-with-tape
    (should-error (superchat-permission-set "new" 'auto))
    (let ((entry (superchat-permission-set "new" 'auto "/p/")))
      (should (equal (plist-get entry :scope) "/p/")))
    (should (eq (superchat-permission-level "new") 'auto))
    ;; Once the timeline has an entry, SCOPE is not given again.
    (superchat-permission-set "new" 'ask "/p/")
    (should (eq (superchat-permission-level "new") 'ask))))

(ert-deftest superchat-permission-set-rejects-unknown-level ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (should-error (superchat-permission-set "t" 'bogus))))

(ert-deftest superchat-permission-ask-calls-the-function ()
  (let (seen)
    (dolist (answer '(allow deny always))
      (let ((superchat-permission-ask-function
             (lambda (tool summary) (push (list tool summary) seen) answer)))
        (should (eq (superchat-permission-ask "shell" "ls") answer))))
    (should (equal (car seen) '("shell" "ls")))))

(ert-deftest superchat-permission-asking-names-the-question-while-it-waits ()
  (let (seen)
    (let ((superchat-permission-ask-function
           (lambda (&rest _) (setq seen superchat-permission-asking) 'allow)))
      (should (eq (superchat-permission-ask "shell" "ls" "t") 'allow)))
    (should (equal seen '(:tape "t" :tool "shell" :summary "ls")))
    (should-not superchat-permission-asking)
    (let ((superchat-permission-ask-function (lambda (&rest _) (signal 'quit nil))))
      (should (eq (condition-case nil (superchat-permission-ask "shell" "ls" "t")
                    (quit 'quit))
                  'quit)))
    (should-not superchat-permission-asking)))

(ert-deftest superchat-permission-level-is-one-query-for-the-newest-level ()
  (superchat-test-with-tape
    (superchat-tape-append "t" 'user "hi" :scope "home")
    (superchat-permission-set "t" 'auto)
    (dotimes (_ 50) (superchat-tape-append "t" 'event "p" :data '(:type "permission")))
    (let* ((rows 0) (entries (symbol-function 'superchat-tape-entries)))
      (cl-letf (((symbol-function 'superchat-tape-entries)
                 (lambda (&rest args)
                   (let ((found (apply entries args))) (cl-incf rows (length found)) found))))
        (should (eq (superchat-permission-level "t") 'auto)))
      (should (= rows 0)))))

(ert-deftest superchat-permission-level-works-before-the-tape-was-opened ()
  (let* ((dir (file-name-as-directory (make-temp-file "superchat-perm-" t)))
         (superchat-directory (expand-file-name "fresh/" dir)) (superchat-tape-file nil))
    (unwind-protect
        (progn (superchat-tape-close)
               (should (eq (superchat-permission-level "x") superchat-default-level))
               (should (eq (superchat-permission-decide "x" 'write) 'ask)))
      (superchat-tape-close)
      (delete-directory dir t))))

(provide 'test-permission)
;;; test-permission.el ends here
