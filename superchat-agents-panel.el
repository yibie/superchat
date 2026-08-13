;;; superchat-agents-panel.el --- TextUI panel for running sub-agents -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The chat buffer remains the durable Superchat surface.  This optional
;; panel is a separate TextUI buffer that observes the sub-agent control
;; plane and gives each running request a direct cancellation button.
;; The render function pulls its rows from `superchat-subagent-running';
;; the sub-agent lifecycle calls `superchat-agents-panel-refresh', so no
;; polling timer is needed.

;;; Code:

(require 'cl-lib)
(require 'superchat-subagent)

(declare-function superchat-subagent-running "superchat-subagent" ())
(declare-function superchat-subagent-cancel "superchat-subagent" (id &optional reason))
(declare-function textui-open "textui" (name render-function &optional initial-state))
(declare-function textui-request-refresh "textui" (buffer))

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
       (fboundp 'textui-request-refresh)))

(defun superchat-agents-panel--row (entry)
  "Return a TextUI flex row for running sub-agent ENTRY."
  (let* ((id (plist-get entry :id))
         (preset (or (plist-get entry :preset) "?"))
         (depth (or (plist-get entry :depth) 0))
         (started-at (or (plist-get entry :started-at) (float-time)))
         (elapsed (max 0 (- (float-time) started-at)))
         (target-id id))
    (list :type :flex :direction :row :gap 2
          :children
          (list
           (list :type :text :value
                 (format "%s  %s  depth=%d  %.1fs"
                         id preset depth elapsed))
           (list :type 'push-button :tag "Cancel"
                 :notify
                 (lambda (&rest _event)
                   (when (fboundp 'superchat-subagent-cancel)
                     (superchat-subagent-cancel target-id))))))))

(defun superchat-agents-panel--main-run-row ()
  "Return a TextUI row for the session's active main Run, or nil.
The paused flag shows as a status marker; the row's Cancel button
uses the interrupt + redirect command."
  (when (and (boundp 'superchat--active-run-id)
             superchat--active-run-id
             (fboundp 'superchat-runtime-active-runs)
             (member superchat--active-run-id
                     (superchat-runtime-active-runs)))
    (let* ((run-id superchat--active-run-id)
           (status (and (fboundp 'superchat-runtime-status)
                        (superchat-runtime-status run-id)))
           (paused (and (boundp 'superchat--pause-requested)
                        superchat--pause-requested))
           (label (format "main  %s  %s%s"
                          run-id
                          (or (plist-get status :status) "?")
                          (if paused "  PAUSED" ""))))
      (list :type :flex :direction :row :gap 2
            :children
            (list
             (list :type :text :value label)
             (list :type 'push-button :tag "Cancel"
                   :notify
                   (lambda (&rest _event)
                     (when (fboundp 'superchat-cancel)
                       (superchat-cancel)))))))))

(defun superchat-agents-panel--frame ()
  "Build the panel frame from the live control planes.
Covers the session's main Run (with its paused marker) and running
sub-agents.  Workflow step state is not yet queryable and stays out
of the panel for now."
  (let* ((entries (and (fboundp 'superchat-subagent-running)
                       (superchat-subagent-running)))
         (main (superchat-agents-panel--main-run-row))
         (rows (delq nil (cons main (mapcar
                                     #'superchat-agents-panel--row
                                     entries)))))
    (if rows
        (cons (list :type :text :value "Running agents and main Run")
              rows)
      (list (list :type :text :value "No running sub-agents.")))))

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

(defun superchat-agents-panel-show ()
  "Render and display the running sub-agent panel.
Returns the panel buffer, or nil when textui.el is unavailable."
  (interactive)
  (let ((buffer-name (superchat-agents-panel--safe-buffer-name)))
    (when (and buffer-name (superchat-agents-panel-available-p))
      (let ((existing (get-buffer buffer-name)))
        (if (and existing
                 (with-current-buffer existing
                   (derived-mode-p 'textui-mode)))
            (progn
              (textui-request-refresh existing)
              (display-buffer existing)
              existing)
          (let ((buffer
                 (textui-open buffer-name
                              (lambda (_width)
                                (superchat-agents-panel--frame)))))
            (when (buffer-live-p buffer)
              (display-buffer buffer))
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
