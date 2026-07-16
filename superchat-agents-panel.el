;;; superchat-agents-panel.el --- VUI panel for running sub-agents -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The chat buffer remains the durable Superchat surface.  This optional
;; panel is a separate VUI buffer that observes the sub-agent control plane
;; and gives each running request a direct cancellation button.

;;; Code:

(require 'cl-lib)
(require 'superchat-subagent)

(declare-function superchat-subagent-running "superchat-subagent" ())
(declare-function superchat-subagent-cancel "superchat-subagent" (id &optional reason))
(declare-function vui-fragment "vui" (&rest children))
(declare-function vui-text "vui" (content &rest props))
(declare-function vui-button "vui" (label &rest props))
(declare-function vui-newline "vui" (&rest props))
(declare-function vui-component "vui" (name &rest props))
(declare-function vui-mount "vui" (component &optional buffer-name))
(declare-function vui-flush-sync "vui" ())
(declare-function vui-refresh "vui" ())

(defgroup superchat-agents-panel nil
  "The optional VUI panel for active Superchat sub-agents."
  :group 'superchat)

(defcustom superchat-agents-panel-buffer-name "*superchat-agents*"
  "Name of the buffer used by the sub-agent panel."
  :type 'string
  :group 'superchat-agents-panel)

(defvar-local superchat-agents-panel--refresh-fn nil
  "Component callback that requests a panel re-render in this buffer.")

(defvar superchat-agents-panel--component-defined nil
  "Non-nil after the VUI component definition has been registered.")

(defun superchat-agents-panel-available-p ()
  "Return non-nil when vui.el is available and the panel can be mounted."
  (and (or (featurep 'vui) (require 'vui nil t))
       (fboundp 'vui-defcomponent)
       (fboundp 'vui-component)
       (fboundp 'vui-mount)
       (fboundp 'vui-text)
       (fboundp 'vui-button)
       (fboundp 'vui-fragment)
       (fboundp 'vui-newline)
       (fboundp 'vui-set-state)
       (fboundp 'vui-with-async-context)))

(defun superchat-agents-panel--row (entry)
  "Return a VUI row for running sub-agent ENTRY."
  (let* ((id (plist-get entry :id))
         (preset (or (plist-get entry :preset) "?"))
         (depth (or (plist-get entry :depth) 0))
         (started-at (or (plist-get entry :started-at) (float-time)))
         (elapsed (max 0 (- (float-time) started-at))))
    (vui-fragment
     (vui-text (format "%s  %s  depth=%d  %.1fs "
                      id preset depth elapsed))
     (vui-button "Cancel"
                 :on-click (lambda ()
                             (when (fboundp 'superchat-subagent-cancel)
                               (superchat-subagent-cancel id))))
     (vui-newline))))

(defun superchat-agents-panel--render (&optional revision)
  "Build the panel VUI tree.
REVISION is only a state dependency; the data comes from the control plane."
  (ignore revision)
  (let ((entries (and (fboundp 'superchat-subagent-running)
                      (superchat-subagent-running))))
    (apply #'vui-fragment
           (append
            (list (vui-text "Running sub-agents" :face 'bold)
                  (vui-newline))
            (if entries
                (mapcar #'superchat-agents-panel--row entries)
              (list (vui-text "No running sub-agents.")
                    (vui-newline)))))))

(defun superchat-agents-panel--define-component ()
  "Define the panel component after vui.el has been loaded."
  (when (and (superchat-agents-panel-available-p)
             (not superchat-agents-panel--component-defined))
    ;; `vui-defcomponent' is a macro.  Evaluate the definition only after
    ;; the optional dependency is present so loading Superchat stays cheap.
    (eval
     '(vui-defcomponent superchat-agents-panel-component ()
        :state ((revision 0))
        :on-mount
        (progn
          (setq superchat-agents-panel--refresh-fn
                (vui-with-async-context
                  (vui-set-state :revision #'1+)))
          nil)
        :on-unmount
        (progn
          (setq superchat-agents-panel--refresh-fn nil)
          nil)
        :render
        (superchat-agents-panel--render revision))
     t)
    (setq superchat-agents-panel--component-defined t))
  superchat-agents-panel--component-defined)

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

The sub-agent lifecycle calls this function; no polling timer is needed.
`vui-flush-sync' is used when provided by the installed vui.el version, with
the public refresh command as a compatibility fallback." 
  (interactive)
  (let ((buffer (get-buffer superchat-agents-panel-buffer-name)))
    (when (and buffer (superchat-agents-panel-available-p))
      (with-current-buffer buffer
        (when (functionp superchat-agents-panel--refresh-fn)
          (funcall superchat-agents-panel--refresh-fn))
        (cond
         ((fboundp 'vui-flush-sync) (vui-flush-sync))
         ((fboundp 'vui-refresh) (vui-refresh)))))))

(defun superchat-agents-panel-show ()
  "Mount and display the running sub-agent panel.
Returns the panel buffer, or nil when vui.el is unavailable."
  (interactive)
  (let ((buffer-name (superchat-agents-panel--safe-buffer-name)))
    (when (and buffer-name (superchat-agents-panel--define-component))
      (save-current-buffer
        (save-selected-window
          (vui-mount
           (vui-component 'superchat-agents-panel-component)
           buffer-name)))
      (let ((buffer (get-buffer buffer-name)))
        (when (buffer-live-p buffer)
          (display-buffer buffer))
        buffer))))

(provide 'superchat-agents-panel)

;;; superchat-agents-panel.el ends here
