;;; test-agents-panel.el --- Tests for the TextUI sub-agent panel -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-agents-panel)
(require 'superchat-subagent)
(require 'superchat-tape-browser)

;; Product variables must be special in this lexical test file.
(defvar superchat-buffer-name)
(defvar superchat--active-run-id)
(defvar superchat--pause-requested)
(defvar superchat-agents-panel-buffer-name)
(defvar superchat--subagent-running)
(defvar textui-state)

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
          ;; the row's cancel control, extracted from the built frame:
          ;; every control routes through the one interaction seam
          (let* ((frame (superchat-agents-panel--frame))
                 (row (cl-find-if
                       (lambda (element)
                         (equal "sub-1"
                                (plist-get
                                 (nth 1 (plist-get element :children))
                                 :target-id)))
                       (plist-get (car frame) :children)))
                 (button (nth 1 (plist-get row :children)))
                 (notify (plist-get button :notify))
                 (w (widget-convert (plist-get button :type)
                                    :panel-action
                                    (plist-get button :panel-action)
                                    :target-id
                                    (plist-get button :target-id))))
            (should (eq #'superchat-agents-panel--view-action notify))
            (cl-letf (((symbol-function 'superchat-subagent-cancel)
                       (lambda (id &optional _reason)
                         (setq cancelled id))))
              (funcall notify w)
              (should (equal cancelled "sub-1")))))
      (when (get-buffer superchat-agents-panel-buffer-name)
        (kill-buffer superchat-agents-panel-buffer-name)))))

(ert-deftest agents-panel/main-run-row-shows-active-and-paused ()
  "The panel reads its selected chat buffer and projects its controls."
  (let ((superchat-buffer-name
         (generate-new-buffer-name "*superchat-run-control-test*"))
        (superchat--subagent-running nil))
    (unwind-protect
        (let ((chat (get-buffer-create superchat-buffer-name)))
          (with-current-buffer chat
            (setq superchat--active-run-id "run-main"
                  superchat--pause-requested nil))
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () '("run-main")))
                    ((symbol-function 'superchat-runtime-status)
                     (lambda (_run-id)
                       (list :run-id "run-main" :status 'running-model
                             :model "model-x" :capability "code"
                             :session-id "session-main"
                             :pending-steer-count 1))))
            (let* ((textui-state (list :chat-buffer-name superchat-buffer-name))
                   (row (superchat-agents-panel--main-run-row))
                   (children (plist-get row :children)))
              (should (string-match-p "run-main"
                                      (plist-get (car children) :value)))
              (should (string-match-p "model-x"
                                      (plist-get (car children) :value)))
              (should (string-match-p "1 steer queued"
                                      (plist-get (car children) :value)))
              
              (should (equal "Pause" (plist-get (nth 1 children) :tag)))
              (should (equal "History" (plist-get (nth 3 children) :tag))))
            (with-current-buffer chat
              (setq superchat--pause-requested t))
            (let* ((textui-state (list :chat-buffer-name superchat-buffer-name))
                   (row (superchat-agents-panel--main-run-row))
                   (children (plist-get row :children)))
              (should (string-match-p "PAUSED"
                                      (plist-get (car children) :value)))
              (should (equal "Steer & Resume"
                             (plist-get (nth 1 children) :tag)))
              (should (equal "History" (plist-get (nth 3 children) :tag))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest agents-panel/shows-undelivered-steer-after-terminal-run ()
  "A terminal Run keeps its undelivered steer warning visible in the panel."
  (let ((superchat-buffer-name
         (generate-new-buffer-name "*superchat-undelivered-steer-test*")))
    (unwind-protect
        (let ((chat (get-buffer-create superchat-buffer-name)))
          (with-current-buffer chat
            (setq superchat--active-run-id "run-finished"))
          (cl-letf (((symbol-function 'superchat-runtime-active-runs)
                     (lambda () nil))
                    ((symbol-function 'superchat-runtime-status)
                     (lambda (_run-id)
                       '(:status completed :model "model-x" :capability code
                         :session-id "session-finished"
                         :undelivered-steer-count 1))))
            (let* ((textui-state (list :chat-buffer-name superchat-buffer-name))
                   (row (superchat-agents-panel--main-run-row))
                   (children (plist-get row :children)))
              (should (string-match-p "1 steer undelivered"
                                      (plist-get (car children) :value)))
              (should (equal "History" (plist-get (nth 1 children) :tag))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest agents-panel/main-controls-target-source-chat-buffer ()
  "Main Run buttons call the existing commands in their source chat buffer."
  (let ((superchat-buffer-name
         (generate-new-buffer-name "*superchat-run-control-action-test*"))
        calls)
    (unwind-protect
        (let ((chat (get-buffer-create superchat-buffer-name)))
          (cl-labels ((button (action)
                        (widget-convert
                         'push-button :panel-action action
                         :chat-buffer-name superchat-buffer-name)))
            (cl-letf (((symbol-function 'superchat-pause)
                       (lambda () (push (list :pause (buffer-name)) calls)))
                      ((symbol-function 'superchat-resume)
                       (lambda () (push (list :resume (buffer-name)) calls)))
                      ((symbol-function 'superchat-cancel)
                       (lambda () (push (list :cancel (buffer-name)) calls))))
              (superchat-agents-panel--view-action (button :pause-main))
              (superchat-agents-panel--view-action (button :resume-main))
              (superchat-agents-panel--view-action (button :cancel-main))
              (should (equal (nreverse calls)
                             (list (list :pause (buffer-name chat))
                                   (list :resume (buffer-name chat))
                                   (list :cancel (buffer-name chat))))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest agents-panel/history-button-opens-run-session ()
  "The History control uses the selected Run's session identity."
  (let ((session-id "session-history")
        opened)
    (cl-letf (((symbol-function 'superchat-session-history)
               (lambda (id) (setq opened id))))
      (superchat-agents-panel--view-action
       (widget-convert 'push-button :panel-action :history-main
                       :session-id session-id))
      (should (equal opened session-id)))))

;;; test-agents-panel.el ends here
