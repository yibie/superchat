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

(ert-deftest genui/accepts-vui-table ()
  "The table layout advertised by the prompt passes the pure gate."
  (should
   (eq t
       (superchat-genui-validate
        '(vui-table
          :columns ((:header "方案") (:header "结论"))
          :rows (("Redux" "成熟、可预测")
                 ("Zustand" "轻量、灵活")
                 ("Context" "适合简单共享状态"))
          :border :ascii)))))

(ert-deftest genui/accepts-interactive-vui-table-cell ()
  "A table row may contain a vnode with a registered safe action."
  (should
   (eq t
       (superchat-genui-validate
        '(vui-table
          :columns ((:header "方案") (:header "操作"))
          :rows (("Redux"
                  (vui-button "询问"
                              :on-click
                              (action send-to-chat :text "解释 Redux"))))
          :border :ascii)))))

(ert-deftest genui/quotes-vui-table-data-before-evaluation ()
  "Table data remains data when the trusted vnode form is evaluated."
  (let* ((form '(vui-table
                 :columns ((:header "方案") (:header "结论"))
                 :rows (("Redux" "成熟、可预测"))
                 :border :ascii))
         (expanded (superchat-genui-expand-actions form)))
    (should (equal (nth 2 expanded)
                   '(quote ((:header "方案") (:header "结论")))))
    (should (equal (nth 4 expanded)
                   '(quote (("Redux" "成熟、可预测")))))))

(ert-deftest genui/accepts-and-normalizes-quoted-vui-table-data ()
  "The VUI docstring's quoted data spelling stays safe and idempotent."
  (let* ((form '(vui-table
                 :columns '((:header "方案") (:header "结论"))
                 :rows '(("Redux" "成熟、可预测"))
                 :border :ascii))
         (expanded (superchat-genui-expand-actions form)))
    (should (eq t (superchat-genui-validate form)))
    (should (equal (nth 2 expanded)
                   '(quote ((:header "方案") (:header "结论")))))
    (should (equal (nth 4 expanded)
                   '(quote (("Redux" "成熟、可预测")))))))

(ert-deftest genui/accepts-advertised-collapsible ()
  "Every constructor in the prompt example must pass the pure gate."
  (should
   (eq t
       (superchat-genui-validate
        '(vui-collapsible
          :title "Details"
          :initially-expanded nil
          (vui-text "Long explanation"))))))

(ert-deftest genui/rejects-red-team-forms ()
  "Every known code-injection shape is rejected before evaluation."
  (dolist (source
           '("(vui-text (progn (delete-file \"~/x\") \"hi\"))"
             "(vui-button \"x\" :on-click (lambda () (shell-command \"rm -rf ~\")))"
             "(vui-button \"x\" :on-click (funcall (intern \"delete-file\")))"
             "(vui-text (eval (read user-input)))"
             "`(vui-text ,(delete-file \"x\"))"
             "(vui-button \"x\" :on-click (action unknown-action))"
             "(vui-table :rows ((\"x\" (vui-button \"bad\" :on-click (lambda () (delete-file \"x\"))))))"
             "(vui-table :rows ((\"x\" (vui-button \"bad\" :on-click (action unknown-action)))))"
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
                 '(vui-table :columns (quote . malformed) :rows nil)
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

(ert-deftest genui/send-to-chat-action-submits-fixed-text ()
  "The chat action submits its literal text without evaluating agent code."
  (let* ((buffer-name (generate-new-buffer-name " *superchat-genui-chat*"))
         (chat (get-buffer-create buffer-name))
         (old-buffer-name (and (boundp 'superchat-buffer-name)
                               superchat-buffer-name))
         sent)
    (unwind-protect
        (progn
          (setq superchat-buffer-name buffer-name)
          (with-current-buffer chat
            (setq-local superchat--prompt-start
                        (progn
                          (insert "* User: ")
                          (point-marker))))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda ()
                       (setq sent
                             (buffer-substring-no-properties
                              superchat--prompt-start (point-max))))))
            (let* ((expanded
                    (superchat-genui-expand-actions
                     '(vui-button "Ask"
                                  :on-click
                                  (action send-to-chat :text "Explain Redux"))))
                   (callback (eval (nth 3 expanded) t)))
              (let ((result (funcall callback)))
                (should (equal result
                               "Sent generated UI selection to chat.")))
              (should (equal sent "Explain Redux"))))
      (setq superchat-buffer-name old-buffer-name)
      (when (buffer-live-p chat)
        (kill-buffer chat))))))

;;; test-genui.el ends here
