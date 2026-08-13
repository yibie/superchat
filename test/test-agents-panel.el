;;; test-agents-panel.el --- Tests for the TextUI sub-agent panel -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-agents-panel)
(require 'superchat-subagent)

;; Product variables must be special in this lexical test file.
(defvar superchat-buffer-name)
(defvar superchat-agents-panel-buffer-name)
(defvar superchat--subagent-running)

(defun test-agents-panel--textui-p ()
  "Return non-nil when the TextUI panel surface is available."
  (or (featurep 'textui) (require 'textui nil t)))

(ert-deftest agents-panel/falls-back-without-textui ()
  "The panel falls back to a text summary when textui.el is absent."
  (skip-unless (not (test-agents-panel--textui-p)))
  (let ((superchat--subagent-running nil))
    (let ((result (superchat--cmd-agents nil nil nil nil nil)))
      (should (eq :echo (plist-get result :type))))))

(ert-deftest agents-panel/finish-refreshes-live-panel ()
  "The sub-agent lifecycle refresh re-renders a live TextUI panel."
  (skip-unless (test-agents-panel--textui-p))
  (let* ((superchat-agents-panel-buffer-name
          (generate-new-buffer-name "*superchat-agents-test*"))
         (superchat-buffer-name "*superchat-chat-agents-sentinel*")
         (superchat--subagent-running
          '(("sub-1" :id "sub-1" :preset "researcher" :depth 1
                     :started-at 0))))
    (unwind-protect
        (progn
          (should (superchat-agents-panel-show))
          (with-current-buffer superchat-agents-panel-buffer-name
            (should (eq major-mode 'textui-mode))
            (should (string-match-p "sub-1" (buffer-string)))
            (should (string-match-p "Cancel" (buffer-string))))
          ;; lifecycle: the agent finishes; the panel re-renders
          (setq superchat--subagent-running nil)
          (superchat-agents-panel-refresh)
          (let ((buffer (get-buffer superchat-agents-panel-buffer-name)))
            (should buffer)))
      (when (get-buffer superchat-agents-panel-buffer-name)
        (kill-buffer superchat-agents-panel-buffer-name)))))

(ert-deftest agents-panel/never-targets-the-chat-buffer ()
  "A customized panel name cannot alias the durable chat buffer."
  (let* ((superchat-buffer-name
          (generate-new-buffer-name "*superchat-chat-panel-target*"))
         (superchat-agents-panel-buffer-name superchat-buffer-name)
         (chat (get-buffer-create superchat-buffer-name)))
    (unwind-protect
        (with-current-buffer chat
          (should-not
           (equal superchat-buffer-name
                  (superchat-agents-panel--safe-buffer-name))))
      (when (buffer-live-p chat)
        (kill-buffer chat)))))

(ert-deftest agents-panel/cancel-button-invokes-cancel ()
  "The panel Cancel button's trusted closure calls the cancel path.
Widget activation itself is interactive-only; batch tests invoke the
closure from the built frame directly."
  (skip-unless (test-agents-panel--textui-p))
  (let* ((superchat-agents-panel-buffer-name
          (generate-new-buffer-name "*superchat-agents-cancel-test*"))
         (superchat-buffer-name "*superchat-chat-cancel-sentinel*")
         (superchat--subagent-running
          '(("sub-1" :id "sub-1" :preset "researcher" :depth 1
                     :started-at 0)))
         cancelled)
    (unwind-protect
        (progn
          (should (superchat-agents-panel-show))
          (with-current-buffer superchat-agents-panel-buffer-name
            (should (eq major-mode 'textui-mode))
            (should (string-match-p "Cancel" (buffer-string))))
          ;; the row's cancel closure, extracted from the built frame
          (let* ((frame (superchat-agents-panel--frame))
                 (row (cadr frame))
                 (button (nth 1 (plist-get row :children)))
                 (notify (plist-get button :notify)))
            (should (functionp notify))
            (cl-letf (((symbol-function 'superchat-subagent-cancel)
                       (lambda (id &optional _reason)
                         (setq cancelled id))))
              (funcall notify nil)
              (should (equal cancelled "sub-1")))))
      (when (get-buffer superchat-agents-panel-buffer-name)
        (kill-buffer superchat-agents-panel-buffer-name)))))

;;; test-agents-panel.el ends here
