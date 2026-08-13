;;; test-genui.el --- Tests for the TextUI generated-UI data gate -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-genui)

;; Product variables must be special in this lexical test file so the
;; dynamic bindings are visible to the trusted closures.
(defvar superchat-buffer-name)
(defvar superchat--prompt-start)

(defun test-genui--good-form ()
  "A valid two-line form exercising every grammar branch."
  '(:type :flex :direction :column
    :children
    ((:type :text :value "Which option?")
     (:type :flex :direction :row
      :children
      ((:type button :label "Ask"
              :action (action send-to-chat :text "Explain Redux"))
       (:type :grid :columns 2 :min-column-width 8
        :children ((:type :text :value "a")
                   (:type :text :value "b"))))))))

(ert-deftest genui/accepts-whitelisted-tree-and-action ()
  "The data grammar accepts every element type and a registered action."
  (should (eq t (superchat-genui-validate (test-genui--good-form))))
  (let ((expanded (superchat-genui-expand-actions (test-genui--good-form))))
    (should (consp expanded))
    ;; The button became a native widget element with a closure, not code.
    (should (functionp
             (plist-get
              (car (plist-get
                    (nth 1 (plist-get expanded :children))
                    :children))
              :notify)))))

(ert-deftest genui/rejects-red-team-forms ()
  "Non-whitelisted shapes are rejected with readable reasons."
  (dolist (bad (list
                '(:type :text :value 42)
                '(:type :flex :direction :sideways :children ())
                '(:type :flex :direction :row :children "not-a-list")
                '(:type :unknown :value "x")
                '(:type :text :value "x" :extra 1)
                '(:type button :label 42 :action (action send-to-chat
                                                         :text "x"))
                '(:type button :action (action send-to-chat :text "x"))
                '(:type button :label "x" :action (action unknown :x 1))
                '(:type button :label "x"
                  :action (action send-to-chat :text
                                  (lambda () (shell-command "rm -rf ~"))))
                '(:type button :label "x" :action (eval-when-compile
                                                   (+ 1 2)))
                '(:type :grid :columns 0 :min-column-width 4 :children ())
                '(:type :text :value "x" . "not-proper")))
    (should (stringp (superchat-genui-validate bad)))
    (should (string-match-p "genui" (superchat-genui-validate bad)))))

(ert-deftest genui/rejects-uninterned-constructors ()
  "An uninterned symbol that merely prints like a type is rejected."
  (let ((fake (make-symbol ":text")))
    (should (stringp (superchat-genui-validate (list :type fake :value "x"))))
    (let ((fake-button (make-symbol "button")))
      (should
       (stringp
        (superchat-genui-validate
         (list :type fake-button :label "x"
               :action (list 'action 'send-to-chat :text "x"))))))))

(ert-deftest genui/rejects-text-properties-and-non-literals ()
  "Strings with text properties and rich values are rejected."
  (should (stringp (superchat-genui-validate
                    (list :type :text :value (propertize "x" 'face 'bold)))))
  (should (stringp (superchat-genui-validate
                    (list :type :text :value #("x" 0 1 (face bold)))))))

(ert-deftest genui/rejects-cyclic-forms ()
  "Cyclic element data is rejected without hanging."
  (let* ((bad (list :type :flex :direction :column :children nil)))
    (setcdr (nthcdr 3 bad) (list (cdr bad)))
    (should (stringp (superchat-genui-validate bad)))))

(ert-deftest genui/validation-does-not-call-action-builders ()
  "Validating must stay pure with respect to the action table."
  (cl-letf (((symbol-function 'superchat-genui--build-send-to-chat)
             (lambda (_params) (error "builder must not run"))))
    (should (eq t (superchat-genui-validate
                   (list :type 'button :label "x"
                         :action (list 'action 'send-to-chat :text "x")))))))

(ert-deftest genui/expander-propagates-action-builder-errors ()
  "A builder error surfaces as a readable rejection, not a partial tree."
  (cl-letf (((symbol-function 'superchat-genui--build-send-to-chat)
             (lambda (_params) (error "boom"))))
    (let ((result (superchat-genui-expand-actions
                   (list :type 'button :label "x"
                         :action (list 'action 'send-to-chat :text "x")))))
      (should (stringp result))
      (should (string-match-p "genui" result)))))

(ert-deftest genui/send-to-chat-action-submits-fixed-text ()
  "The trusted send-to-chat closure submits only its fixed prompt."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-genui-send-test*"))
         (chat (get-buffer-create superchat-buffer-name))
         (superchat--prompt-start nil)
         sent)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (erase-buffer)
            (insert "* User: ")
            (setq superchat--prompt-start (point-marker)))
          (let ((form (list :type 'button :label "Ask"
                            :action (list 'action 'send-to-chat
                                          :text "Explain Redux"))))
            (let ((expanded (superchat-genui-expand-actions form)))
              (should (functionp
                       (plist-get expanded :notify)))
              (cl-letf (((symbol-function 'superchat-send-input)
                         (lambda ()
                           (setq sent
                                 (buffer-substring-no-properties
                                  superchat--prompt-start (point-max))))))
                (funcall (plist-get expanded :notify) nil))
              (should (equal sent "Explain Redux")))))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

;;; test-genui.el ends here
