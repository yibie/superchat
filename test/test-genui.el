;;; test-genui.el --- Red-team tests for generated UI validation -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-genui)

(defvar read-eval)

(defun test-genui--read-first (source)
  "Read only the first SOURCE form with recursive/read-time eval disabled."
  (let ((read-eval nil)
        (read-circle nil))
    (car (read-from-string source))))

(ert-deftest genui/accepts-whitelisted-tree-and-action ()
  "A vnode tree with a registered action passes the pure gate."
  (should
   (eq t
       (superchat-genui-validate
        '(vui-fragment
          (vui-text "hi")
          (vui-button "Cancel"
                       :on-click (action cancel-agent :id "sub-1")))))))

(ert-deftest genui/rejects-red-team-forms ()
  "Every known code-injection shape is rejected before evaluation."
  (dolist (source
           '("(vui-text (progn (delete-file \"~/x\") \"hi\"))"
             "(vui-button \"x\" :on-click (lambda () (shell-command \"rm -rf ~\")))"
             "(vui-button \"x\" :on-click (funcall (intern \"delete-file\")))"
             "(vui-text (eval (read user-input)))"
             "`(vui-text ,(delete-file \"x\"))"
             "(vui-button \"x\" :on-click (action unknown-action))"
             "(unknown-constructor \"x\")"))
    (let ((result (superchat-genui-validate (test-genui--read-first source))))
      (should (stringp result))
      (should (string-match-p "genui" result)))))

(ert-deftest genui/rejects-uninterned-constructors ()
  "A symbol that merely prints like a constructor is not enough."
  (should (stringp
           (superchat-genui-validate
            (list (make-symbol "vui-text") "not allowed")))))

(ert-deftest genui/rejects-text-properties-and-non-literals ()
  "Text properties, vectors, and records cannot smuggle executable values."
  (dolist (form
           (list (list 'vui-text
                       (test-genui--read-first
                        "#(\"x\" 0 1 (display (eval (message \"pwn\"))))"))
                 '(vui-text #("x" 0 1 (keymap (keymap (mouse-1 . delete-file)))))
                 '(vui-text ["x"])
                 '(vui-text (quote delete-file))))
    (should (stringp (superchat-genui-validate form)))))

(ert-deftest genui/rejects-malformed-lists-and-actions ()
  "Dotted lists, missing handlers, and odd action plists are rejected."
  (dolist (form
           (list '(vui-text . "x")
                 '(vui-button "x" :on-click)
                 '(vui-button "x" :on-click nil)
                 '(vui-button "x" :on-click (action cancel-agent :id))
                 '(vui-button "x" :on-click (action cancel-agent :id "x" extra))
                 '(vui-button "x" :on-click (action . foo))
                 '(vui-text :class (vui-text "nested"))
                 '(vui-text :class)))
    (should (stringp (superchat-genui-validate form)))))

(ert-deftest genui/rejects-cyclic-forms ()
  "Programmatic callers cannot make validation recurse forever."
  (let* ((form (list 'vui-text nil))
         (tail (cdr form)))
    (setcdr tail tail)
    (should (stringp (superchat-genui-validate form)))))

(ert-deftest genui/read-boundary-uses-only-first-form ()
  "A second form after a valid tree is not part of the accepted UI."
  (let ((form (test-genui--read-first
               "(vui-text \"ok\") (delete-file \"/tmp/should-not-run\")")))
    (should (eq t (superchat-genui-validate form)))))

(ert-deftest genui/read-boundary-disables-read-time-eval ()
  "The Emacs reader rejects read-time evaluation before validation."
  (should-error (test-genui--read-first "#.(message \"must not run\")")
                :type 'invalid-read-syntax))

(ert-deftest genui/validation-does-not-call-action-builders ()
  "The pure validator never invokes trusted action builders."
  (let ((calls 0)
        (superchat-genui-actions
         '((cancel-agent . (lambda (_params) (cl-incf calls)))))
        (form '(vui-button "Cancel"
                          :on-click (action cancel-agent :id "sub-1"))))
    (should (eq t (superchat-genui-validate form)))
    (should (= calls 0))))

(ert-deftest genui/expander-produces-a-quoted-handler ()
  "Expansion creates a trusted callback without evaluating agent code."
  (let* ((superchat-genui-actions
          '((cancel-agent . (lambda (_params) (lambda () "cancelled")))))
         (expanded
          (superchat-genui-expand-actions
           '(vui-button "Cancel"
                        :on-click (action cancel-agent :id "sub-1")))))
    (should (consp expanded))
    (should (equal (funcall (eval (nth 3 expanded) t)) "cancelled"))))

(ert-deftest genui/expander-propagates-action-builder-errors ()
  "Invalid action parameters remain rejection strings after expansion."
  (let ((expanded
         (superchat-genui-expand-actions
          '(vui-button "Cancel"
                       :on-click (action cancel-agent)))))
    (should (stringp expanded))
    (should (string-match-p "genui" expanded))))

;;; test-genui.el ends here
