;;; test-genui-render.el --- Tests for the render_ui tool -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-tools)
(require 'superchat-prompt-hooks)

(declare-function vui-flush-sync "vui" ())

(defun test-genui-render--tool-names (tools)
  "Extract llm tool names from mocked or real TOOLS."
  (mapcar (lambda (tool) (plist-get (cdr tool) :name))
          (cl-remove-if-not #'consp tools)))

(defun test-genui-render--mock-llm-make-tool (&rest args)
  "Return a small plist-shaped mock tool for ARGS."
  (cons 'mock-tool args))

(ert-deftest genui-render/is-default-in-tool-registry ()
  "The render tool is scoped, not global: absent from the small default
allowlist, available to any explicit tool profile that names it."
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
  "The system prompt names constructors and registered actions."
  (let ((superchat-llm-tool-names '("render_ui"))
        (turn (superchat-turn-new "draw a panel")))
    (setf (superchat-turn-system-prompt turn) "base")
    (superchat-prompt-hook--generative-ui turn)
    (should (string-match-p "render_ui"
                            (superchat-turn-system-prompt turn)))
    (should (string-match-p "cancel-agent"
                            (superchat-turn-system-prompt turn)))
    (should (string-match-p "send-to-chat"
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
                  (superchat-tool--generated-ui-buffer-name))))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest genui-render/without-vui-is-a-clean-error ()
  "The optional dependency is reported without touching the chat buffer."
  (skip-unless (not (or (featurep 'vui) (require 'vui nil t))))
  (should (equal "Error: vui.el not available"
                 (superchat-tool-render-ui "(vui-text \"hello\")"))))

(ert-deftest genui-render/reader-controls-end-before-vui-load ()
  "The trusted VUI loading path sees its normal reader settings."
  (skip-unless (or (featurep 'vui) (require 'vui nil t)))
  (let ((read-eval t)
        (read-circle t)
        observed)
    (cl-letf (((symbol-function 'superchat-tool--render-ui-form)
               (lambda (_form)
                 (setq observed (list read-eval read-circle))
                 "Rendered")))
      (should (equal "Rendered"
                     (superchat-tool-render-ui "(vui-text \"hello\")")))
      (should (equal '(t t) observed)))))

(ert-deftest genui-render/vui-table-quotes-plain-data ()
  "A raw :columns/:rows table form reaches VUI as data, not a call."
  (skip-unless (and (or (featurep 'vui) (require 'vui nil t))
                    (fboundp 'vui-mount)))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*superchat-generated-ui-table-test*"))
         (superchat-buffer-name "*superchat-chat-table-sentinel*")
         (result
          (superchat-tool-render-ui
           "(vui-table :columns ((:header \"方案\") (:header \"结论\")) :rows ((\"Redux\" \"成熟、可预测\") (\"Zustand\" \"轻量、灵活\")) :border :ascii)")))
    (unwind-protect
        (progn
          (should (equal result
                         (format "Rendered UI in %s"
                                 superchat-genui-buffer-name)))
          (with-current-buffer superchat-genui-buffer-name
            (should (string-match-p "方案" (buffer-string)))
            (should (string-match-p "Redux" (buffer-string)))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name)))))

(ert-deftest genui-render/vui-table-button-sends-to-chat ()
  "A safe button vnode in a table cell can submit a fixed chat prompt."
  (skip-unless (and (or (featurep 'vui) (require 'vui nil t))
                    (fboundp 'vui-flush-sync)
                    (fboundp 'button-at)
                    (fboundp 'button-activate)))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*superchat-generated-ui-table-action*"))
         (superchat-buffer-name
          (generate-new-buffer-name "*superchat-chat-table-action*"))
         (chat (get-buffer-create superchat-buffer-name))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (erase-buffer)
            (insert "* User: ")
            (setq superchat--prompt-start (point-marker)))
          (cl-letf (((symbol-function 'superchat-send-input)
                     (lambda ()
                       (setq sent
                             (buffer-substring-no-properties
                              superchat--prompt-start (point-max))))))
            (should
             (string-match-p
              "Rendered UI"
              (superchat-tool-render-ui
               "(vui-table :columns ((:header \"方案\") (:header \"操作\")) :rows ((\"Redux\" (vui-button \"询问\" :on-click (action send-to-chat :text \"解释 Redux\")))) :border :ascii)")))
            (with-current-buffer superchat-genui-buffer-name
              (goto-char (point-min))
              (search-forward "询问")
              (button-activate (button-at (1- (point))))
              (vui-flush-sync))
            (should (equal sent "解释 Redux"))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest genui-render/vui-text-button-uses-native-mouse-binding ()
  "Generated text buttons must not fall through to widget mouse handling."
  (skip-unless (and (or (featurep 'vui) (require 'vui nil t))
                    (fboundp 'button-at)))
  (let ((superchat-genui-buffer-name
         (generate-new-buffer-name "*superchat-generated-ui-mouse-test*"))
        (superchat-buffer-name "*superchat-chat-mouse-sentinel*"))
    (unwind-protect
        (progn
          (should
           (string-match-p
            "Rendered UI"
            (superchat-tool-render-ui
             "(vui-button \"Ask\" :on-click (action send-to-chat :text \"Explain\"))")))
          (with-current-buffer superchat-genui-buffer-name
            (goto-char (point-min))
            (search-forward "Ask")
            (backward-char)
            (should (button-at (point)))
            (should (eq (key-binding [down-mouse-1])
                        #'mouse-drag-region))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name)))))

(ert-deftest genui-render/vui-end-to-end-cancel ()
  "A generated button is mounted independently and invokes its safe action."
  (skip-unless (and (or (featurep 'vui) (require 'vui nil t))
                    (fboundp 'vui-flush-sync)
                    (fboundp 'button-at)
                    (fboundp 'button-activate)))
  ;; Keep this assertion before the first valid render: it proves the tool
  ;; can load vui-components after the restricted reader scope has ended.
  (should-not (featurep 'vui-components))
  (let* ((superchat-genui-buffer-name
          (generate-new-buffer-name "*superchat-generated-ui-test*"))
         (superchat-buffer-name "*superchat-chat-sentinel*")
         (superchat--subagent-running
          '(("sub-1" :id "sub-1" :preset "researcher" :depth 1
                     :started-at 0 :callback ignore)))
         (chat (get-buffer-create superchat-buffer-name))
         cancelled)
    (unwind-protect
        (progn
          (with-current-buffer chat
            (erase-buffer)
            (insert "chat must stay unchanged"))
          (cl-letf (((symbol-function 'superchat-subagent-cancel)
                     (lambda (id &optional _reason)
                       (setq cancelled id)
                       (setq superchat--subagent-running nil))))
            (let ((rejected
                   (superchat-tool-render-ui
                    "(vui-button \"x\" :on-click (lambda () (shell-command \"rm -rf ~\")))")))
              (should (string-match-p "genui" rejected)))
            (let ((rejected
                   (superchat-tool-render-ui
                    "(vui-button \"x\" :on-click (action cancel-agent))")))
              (should (string-match-p "genui" rejected))
              (should-not (string-match-p "Rendered UI" rejected)))
            (let (result)
              (with-current-buffer chat
                (setq result
                      (superchat-tool-render-ui
                       "(vui-fragment (vui-text \"Agents\") (vui-button \"Cancel\" :on-click (action cancel-agent :id \"sub-1\")))"))
                (should (eq (current-buffer) chat)))
              (should (string-match-p "Rendered UI" result)))
            (with-current-buffer superchat-genui-buffer-name
              (goto-char (point-min))
              (search-forward "Cancel")
              (button-activate (button-at (1- (point))))
              (vui-flush-sync)
              (should (equal cancelled "sub-1")))
            (with-current-buffer chat
              (should (equal (buffer-string) "chat must stay unchanged")))))
      (when (get-buffer superchat-genui-buffer-name)
        (kill-buffer superchat-genui-buffer-name))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

;;; test-genui-render.el ends here
