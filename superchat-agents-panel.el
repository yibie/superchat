;;; superchat-agents-panel.el --- TextUI panel for running sub-agents -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The chat buffer remains the durable Superchat surface.  This optional
;; panel is a separate TextUI buffer that observes the active main Run and
;; sub-agent control planes.  It provides the main Run's pause/resume/cancel
;; controls without owning any lifecycle state or polling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-subagent)
(require 'superchat-widgets)

(declare-function superchat-subagent-running "superchat-subagent" ())
(declare-function superchat-subagent-cancel "superchat-subagent" (id &optional reason))
(declare-function superchat-session-history "superchat-tape-browser" (session-id))
(declare-function textui-open "textui" (name render-function &optional initial-state))
(declare-function textui-update "textui" (buffer updater &rest keys))
(declare-function textui-request-refresh "textui" (buffer))

(defvar superchat-buffer-name)
(defvar textui-state)
(defvar superchat-agents-panel--view-action-active nil
  "Non-nil while a panel control is synchronously changing Run state.")

(defgroup superchat-agents-panel nil
  "The optional TextUI panel for active Superchat sub-agents."
  :group 'superchat)

(defcustom superchat-agents-panel-buffer-name "*superchat-agents*"
  "Name of the buffer used by the sub-agent panel."
  :type 'string
  :group 'superchat-agents-panel)

(defun superchat-agents-panel-available-p ()
  "Return non-nil when textui.el is available and the panel can render."
  (and (or (featurep 'textui) (require 'textui nil t))
       (fboundp 'textui-open)
       (fboundp 'textui-update)
       (fboundp 'textui-request-refresh)))

(defun superchat-agents-panel--chat-buffer ()
  "Return the chat buffer selected for this panel's TextUI state."
  (let ((name (or (plist-get textui-state :chat-buffer-name)
                  (and (boundp 'superchat-buffer-name)
                       superchat-buffer-name))))
    (and (stringp name) (get-buffer name))))

(defun superchat-agents-panel--call-main-action (widget command)
  "Call COMMAND in WIDGET's source chat buffer."
  (let ((chat (get-buffer (widget-get widget :chat-buffer-name))))
    (when (and (buffer-live-p chat) (fboundp command))
      (with-current-buffer chat
        (funcall command)))))

;; All panel controls route through the one interaction seam; each
;; control only declares its intent with :panel-action.
(defun superchat-agents-panel--view-action (widget &rest _)
  "Single interaction seam for agents panel controls."
  (let ((superchat-agents-panel--view-action-active t))
    (pcase (widget-get widget :panel-action)
      (:cancel-subagent
       (let ((id (widget-get widget :target-id)))
         (when (and id (fboundp 'superchat-subagent-cancel))
           (superchat-subagent-cancel id))))
      (:pause-main
       (superchat-agents-panel--call-main-action widget 'superchat-pause))
      (:resume-main
       (superchat-agents-panel--call-main-action widget 'superchat-resume))
      (:cancel-main
       (superchat-agents-panel--call-main-action widget 'superchat-cancel))
      (:history-main
       (when-let* ((session-id (widget-get widget :session-id)))
         (when (fboundp 'superchat-session-history)
           (superchat-session-history session-id)))))))

(defun superchat-agents-panel--row (entry)
  "Return a TextUI flex row for running sub-agent ENTRY."
  (let* ((id (plist-get entry :id))
         (preset (or (plist-get entry :preset) "?"))
         (depth (or (plist-get entry :depth) 0))
         (started-at (or (plist-get entry :started-at) (float-time)))
         (elapsed (max 0 (- (float-time) started-at))))
    (list :type :flex :direction :row :gap 2
          :children
          (list
           (list :type :text :value
                 (format "%s  %s  depth=%d  %.1fs"
                         id preset depth elapsed))
           (superchat-widgets-button
            "Cancel"
            #'superchat-agents-panel--view-action
            :panel-action :cancel-subagent
            :target-id id)))))

(defun superchat-agents-panel--main-run-row ()
  "Return a TextUI row for the session's active main Run, or nil.
The paused flag selects Resume; the Run's Tape history remains reachable."
  (when-let* ((chat (superchat-agents-panel--chat-buffer)))
    (with-current-buffer chat
      (let* ((run-id (and (boundp 'superchat--active-run-id)
                          superchat--active-run-id))
             (active (and run-id
                          (fboundp 'superchat-runtime-active-runs)
                          (member run-id (superchat-runtime-active-runs))))
             (status (and run-id
                          (fboundp 'superchat-runtime-status)
                          (superchat-runtime-status run-id)))
             (paused (and active
                          (boundp 'superchat--pause-requested)
                          superchat--pause-requested))
             (pending (or (plist-get status :pending-steer-count) 0))
             (undelivered (or (plist-get status :undelivered-steer-count) 0))
             (session-id (plist-get status :session-id))
             (history (and session-id (fboundp 'superchat-session-history)))
             (chat-name (buffer-name chat))
             (label (if run-id
                        (format "Run %s · %s · %s · %s%s"
                                run-id
                                (or (plist-get status :status) "?")
                                (or (plist-get status :model) "backend default")
                                (or (plist-get status :capability) "chat")
                                (concat
                                 (if paused " · PAUSED" "")
                                 (if (> pending 0)
                                     (format " · %d steer queued" pending) "")
                                 (if (> undelivered 0)
                                     (format " · %d steer undelivered" undelivered) "")))
                      "No active Run")))
        (when (or active (> undelivered 0))
          (list :type :flex :direction :row :gap 2
                :children
                (append
                 (list (list :type :text :value label))
                 (when active
                   (list
                    (superchat-widgets-button
                     (if paused "Steer & Resume" "Pause")
                     #'superchat-agents-panel--view-action
                     :panel-action (if paused :resume-main :pause-main)
                     :chat-buffer-name chat-name)
                    (superchat-widgets-button
                     "Cancel"
                     #'superchat-agents-panel--view-action
                     :panel-action :cancel-main
                     :chat-buffer-name chat-name)))
                 (when history
                   (list
                    (superchat-widgets-button
                     "History"
                     #'superchat-agents-panel--view-action
                     :panel-action :history-main
                     :session-id session-id))))))))))

(defun superchat-agents-panel--frame ()
  "Build the panel frame from the live control planes.
Covers the selected session's main Run and running sub-agents."
  (let* ((entries (and (fboundp 'superchat-subagent-running)
                       (superchat-subagent-running)))
         (main (superchat-agents-panel--main-run-row))
         (subagent-rows (mapcar #'superchat-agents-panel--row entries)))
    (list
     (list :type :flex :direction :column :gap 1
           :children
           (append
            (list (list :type :text :value "Run Control"))
            (if main
                (list main)
              (list (list :type :text :value "No active main Run.")))
            (when subagent-rows
              (cons (list :type :text :value "Subagents") subagent-rows)))))))

(defun superchat-agents-panel--safe-buffer-name ()
  "Return the configured panel name unless it aliases the chat surface."
  (let* ((name superchat-agents-panel-buffer-name)
         (existing (and (stringp name) (get-buffer name))))
    (unless (or (not (stringp name))
                (string-empty-p name)
                (and (boundp 'superchat-buffer-name)
                     (equal name superchat-buffer-name))
                (and existing
                     (boundp 'superchat-mode)
                     (buffer-local-value 'superchat-mode existing)))
      name)))

(defun superchat-agents-panel-refresh ()
  "Refresh the panel if its buffer is currently alive.

The sub-agent lifecycle calls this function; no polling timer is needed."
  (interactive)
  (let ((buffer (get-buffer superchat-agents-panel-buffer-name)))
    (when (and buffer (superchat-agents-panel-available-p))
      (with-current-buffer buffer
        (when (derived-mode-p 'textui-mode)
          (textui-request-refresh buffer))))))

(defun superchat-agents-panel--refresh-after-turn (_status)
  "Refresh a visible panel after an externally completed main Run."
  (unless superchat-agents-panel--view-action-active
    (superchat-agents-panel-refresh)))

(add-hook 'superchat-turn-completed-functions
          #'superchat-agents-panel--refresh-after-turn)

;;;###autoload
(defun superchat-agents-panel-show ()
  "Render and display the running sub-agent panel.
Returns the panel buffer, or nil when textui.el is unavailable."
  (interactive)
  (let* ((buffer-name (superchat-agents-panel--safe-buffer-name))
         (chat-buffer-name
          (if (bound-and-true-p superchat-mode)
              (buffer-name)
            superchat-buffer-name))
         (state (list :chat-buffer-name chat-buffer-name)))
    (when (and buffer-name (superchat-agents-panel-available-p))
      (let ((existing (get-buffer buffer-name)))
        (if (and existing
                 (with-current-buffer existing
                   (derived-mode-p 'textui-mode)))
            (progn
              (textui-update existing (lambda (_state) state))
              (superchat-side-window-show existing 'agents)
              existing)
          (let ((buffer
                 (textui-open buffer-name
                              (lambda (_width)
                                (superchat-agents-panel--frame))
                              state)))
            (when (buffer-live-p buffer)
              (superchat-side-window-show buffer 'agents))
            buffer))))))

;;;###autoload
(defun superchat--cmd-agents (_cmd _args _input _lang _target-model)
  "Show active sub-agents in the TextUI panel when available.
Fall back to the existing text summary when textui.el is absent."
  (if (and (superchat-agents-panel-available-p)
           (superchat-agents-panel-show))
      '(:type :noop)
    `(:type :echo :content ,(superchat-subagent-list-running))))

(provide 'superchat-agents-panel)

;;; superchat-agents-panel.el ends here
