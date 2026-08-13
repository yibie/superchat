;;; test-genui-render.el --- Tests for the render_ui tool over TextUI -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-tools)
(require 'superchat-prompt-hooks)

;; Product variables must be special in this lexical test file.
(defvar superchat-buffer-name)
(defvar superchat-genui-buffer-name)
(defvar superchat--prompt-start)

(defun test-genui-render--tool-names (tools)
  "Extract llm tool names from mocked or real TOOLS."
  (mapcar (lambda (tool) (plist-get (cdr tool) :name))
          (cl-remove-if-not #'consp tools)))

(defun test-genui-render--mock-llm-make-tool (&rest args)
  "Return a small plist-shaped mock tool for ARGS."
  (cons 'mock-tool args))

(ert-deftest genui-render/is-default-in-tool-registry ()
  "The render tool is scoped, not global."
  (should-not (member "render_ui"
                      (default-value 'superchat-llm-tool-names)))
  (let ((superchat-llm-tools-list nil)
        (superchat--llm-tool-names-signature nil)
        (superchat-llm-tool-names
         '("read-file" "list-files" "search-text" "read_buffer")))
    (cl-letf (((symbol-function 'llm-make-tool)
               #'test-genui-render--mock-llm-make-tool))
      (should-not (member "render_ui"
                          (test-genui-render--tool-names
                           (superchat-llm-tools-reload))))))
  (let ((superchat-llm-tools-list nil)
        (superchat--llm-tool-names-signature nil)
        (superchat-llm-tool-names '("render_ui")))
    (cl-letf (((symbol-function 'llm-make-tool)
               #'test-genui-render--mock-llm-make-tool))
      (should (member "render_ui"
                      (test-genui-render--tool-names
                       (superchat-llm-tools-reload)))))))

(ert-deftest genui-render/prompt-hook-advertises-surface ()
  "The system prompt names the data grammar and registered actions."
  (let ((superchat-llm-tool-names '("render_ui"))
        (turn (superchat-turn-new "draw a panel")))
    (setf (superchat-turn-system-prompt turn) "base")
    (superchat-prompt-hook--generative-ui turn)
    (should (string-match-p "render_ui"
                            (superchat-turn-system-prompt turn)))
    (should (string-match-p "cancel-agent"
                            (superchat-turn-system-prompt turn)))
    (should (string-match-p "send-to-chat"
                            (superchat-turn-system-prompt turn)))
    (should (string-match-p ":type :flex"
                            (superchat-turn-system-prompt turn)))))

(ert-deftest genui-render/prompt-hook-is-silent-when-disabled ()
  "The capability does not leak into prompts without the active tool."
  (let ((superchat-llm-tool-names nil)
        (turn (superchat-turn-new "draw a panel")))
    (setf (superchat-turn-system-prompt turn) "base")
    (superchat-prompt-hook--generative-ui turn)
    (should (equal "base" (superchat-turn-system-prompt turn)))))

(ert-deftest genui-render/never-targets-the-chat-buffer ()
  "A customized generated-UI name cannot alias the durable chat buffer."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-chat-target-test*"))
         (superchat-genui-buffer-name superchat-buffer-name)
         (chat (get-buffer-create superchat-buffer-name)))
    (unwind-protect
        (with-current-buffer chat
          (should-not
           (equal superchat-buffer-name
                  (superchat-genui--safe-buffer-name))))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest genui-render/without-textui-is-a-clean-error ()
  "The optional dependency is reported without touching the chat buffer."
  (skip-unless (not (or (featurep 'textui) (require 'textui nil t))))
  (should (equal "Error: textui.el not available"
                 (superchat-tool-render-ui
                  "(:type :text :value \"hello\")"))))

(ert-deftest genui-render/reader-controls-end-before-textui-load ()
  "The trusted loading path sees its normal reader settings."
  (skip-unless (or (featurep 'textui) (require 'textui nil t)))
  (let ((read-eval t)
        (read-circle t)
        observed)
    (cl-letf (((symbol-function 'superchat-tool--render-ui-form)
               (lambda (_form)
                 (setq observed (list read-eval read-circle))
                 "Rendered")))
      (should (equal "Rendered"
                     (superchat-tool-render-ui
                      "(:type :text :value \"hello\")")))
      (should (equal '(t t) observed)))))

(ert-deftest genui-render/renders-into-textui-buffer ()
  "A valid component renders into a textui-mode buffer."
  (skip-unless (or (featurep 'textui) (require 'textui nil t)))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*superchat-generated-ui-e2e*"))
         (superchat-buffer-name "*superchat-chat-sentinel*")
         (result
          (superchat-tool-render-ui
           (prin1-to-string
            '(:type :flex :direction :column
              :children ((:type :text :value "方案 A")
                         (:type button :label "询问"
                                :action (action send-to-chat
                                                :text "Explain Redux"))))))))
    (unwind-protect
        (progn
          (should (equal result superchat-genui-buffer-name))
          (with-current-buffer superchat-genui-buffer-name
            (should (eq major-mode 'textui-mode))
            (should (string-match-p "方案 A" (buffer-string)))
            (should (string-match-p "询问" (buffer-string)))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name)))))

(ert-deftest genui-render/auto-wraps-multiple-forms-in-a-column ()
  "Several top-level forms become one column flex with a hint."
  (skip-unless (or (featurep 'textui) (require 'textui nil t)))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*superchat-generated-ui-wrap*"))
         (superchat-buffer-name "*superchat-chat-wrap-sentinel*")
         (result
          (superchat-tool-render-ui
           "(:type :text :value \"one\") (:type :text :value \"two\")")))
    (unwind-protect
        (progn
          (should (string-match-p "auto-wrapped" result))
          (with-current-buffer superchat-genui-buffer-name
            (should (string-match-p "one" (buffer-string)))
            (should (string-match-p "two" (buffer-string)))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name)))))

(ert-deftest genui-render/end-to-end-cancel ()
  "A generated button renders in the TextUI buffer and its trusted
closure performs the cancel action.  Widget activation itself is
interactive-only; batch tests exercise the closure directly."
  (skip-unless (or (featurep 'textui) (require 'textui nil t)))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*superchat-generated-ui-cancel*"))
         (superchat-buffer-name "*superchat-chat-cancel-sentinel*")
         (chat (get-buffer-create superchat-buffer-name))
         cancelled)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (erase-buffer)
            (insert "chat must stay unchanged"))
          (cl-letf (((symbol-function 'superchat-subagent-cancel)
                     (lambda (id &optional _reason)
                       (setq cancelled id))))
            (let ((rejected
                   (superchat-tool-render-ui
                    (prin1-to-string
                     '(:type button :label "x" :action
                       (action cancel-agent)))))
                  (rejected2
                   (superchat-tool-render-ui
                    (prin1-to-string
                     '(:type button :label "x" :action
                       (action cancel-agent :id 42))))))
              (should (string-match-p "genui" rejected))
              (should (string-match-p "genui" rejected2)))
            (let ((result
                   (superchat-tool-render-ui
                    (prin1-to-string
                     '(:type button :label "Cancel" :action
                       (action cancel-agent :id "sub-1"))))))
              (should (equal result superchat-genui-buffer-name)))
            (with-current-buffer superchat-genui-buffer-name
              (should (eq major-mode 'textui-mode))
              (should (string-match-p "Cancel" (buffer-string))))
            ;; the trusted closure itself performs the action
            (let ((expanded
                   (superchat-genui-expand-actions
                    '(:type button :label "Cancel" :action
                      (action cancel-agent :id "sub-1")))))
              (funcall (plist-get expanded :notify) nil)
              (should (equal cancelled "sub-1")))
            (with-current-buffer chat
              (should (equal (buffer-string) "chat must stay unchanged")))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest genui-render/side-window-display ()
  "render_ui displays the UI in a right side window beside the chat."
  (skip-unless (or (featurep 'textui) (require 'textui nil t)))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*genui-side-test*"))
         (superchat-buffer-name "*superchat-chat-side-sentinel*")
         action-seen)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer &optional action)
                       (when action (setq action-seen action))
                       nil)))
            (superchat-genui-render
             '(:type :text :value "hello")))
          (should action-seen)
          (should (assq 'side action-seen))
          (should (eq 'right (cdr (assq 'side action-seen)))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name)))))

;;; test-genui-render.el ends here
