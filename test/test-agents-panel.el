;;; test-agents-panel.el --- Tests for the optional sub-agent panel -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-agents-panel)

(ert-deftest agents-panel/falls-back-without-vui ()
  "`/agents' keeps the text summary when VUI is unavailable."
  (let ((superchat--subagent-running nil))
    (cl-letf (((symbol-function 'superchat-agents-panel-available-p)
               (lambda () nil)))
      (let ((result (superchat--cmd-agents "agents" nil nil nil nil)))
        (should (eq (plist-get result :type) :echo))
        (should (string-match-p "No running sub-agents"
                                (plist-get result :content)))))))

(ert-deftest agents-panel/finish-refreshes-live-panel ()
  "Finishing a run notifies the panel exactly once."
  (let ((superchat--subagent-running
         '(("sub-1" :id "sub-1" :callback ignore)))
        (refreshes 0))
    (cl-letf (((symbol-function 'superchat-agents-panel-refresh)
               (lambda () (cl-incf refreshes))))
      (superchat--subagent-finish "sub-1" "done")
      (should (= refreshes 1))
      (should-not superchat--subagent-running))))

(ert-deftest agents-panel/vui-mount-and-cancel ()
  "The VUI panel renders a row and its button cancels that row."
  (skip-unless (and (superchat-agents-panel-available-p)
                    (fboundp 'vui-flush-sync)))
  (let* ((superchat-agents-panel-buffer-name
          (generate-new-buffer-name "*superchat-agents-test*"))
         (superchat--subagent-running
          '(("sub-1" :id "sub-1" :preset "researcher" :depth 1
                     :started-at 0 :callback ignore)))
         cancelled)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'superchat-subagent-cancel)
                     (lambda (id &optional _reason)
                       (setq cancelled id)
                       (setq superchat--subagent-running nil)
                       (superchat-agents-panel-refresh))))
            (superchat-agents-panel-show)
            (with-current-buffer superchat-agents-panel-buffer-name
              (goto-char (point-min))
              (search-forward "Cancel")
              (backward-char (length "Cancel"))
              (push-button)
              (vui-flush-sync)
              (should (equal cancelled "sub-1"))
              (should-not (string-match-p "sub-1" (buffer-string))))))
      (when (get-buffer superchat-agents-panel-buffer-name)
        (kill-buffer superchat-agents-panel-buffer-name)))))

;;; test-agents-panel.el ends here
