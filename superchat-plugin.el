;;; superchat-plugin.el --- define, enable, disable a plugin -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'superchat)

(defvar superchat-plugin--definitions nil
  "Alist of (NAME . (:doc S :enable F :disable F)).")

(defvar superchat-plugin--enabled nil
  "Names of the plugins whose ENABLE has run and DISABLE has not.")

(defun superchat-plugin-enabled-p (name)
  "Non-nil when plugin NAME is enabled."
  (and (memq name superchat-plugin--enabled) t))

(defun superchat-plugin--definition (name)
  "Return the definition of NAME, or signal an error naming it."
  (or (cdr (assq name superchat-plugin--definitions))
      (error "No plugin is defined with the name `%s'" name)))

(cl-defun superchat-plugin-define (name &key doc enable disable)
  "Define plugin NAME with DOC, ENABLE and DISABLE; do not enable it.
Redefining an enabled plugin disables the old definition first."
  (when (assq name superchat-plugin--definitions)
    (superchat-plugin-disable name))
  (setf (alist-get name superchat-plugin--definitions)
        (list :doc doc :enable enable :disable disable))
  name)

(defun superchat-plugin-enable (name)
  "Enable plugin NAME by calling its ENABLE once.
An error from ENABLE reaches the caller and leaves NAME not enabled."
  (let ((definition (superchat-plugin--definition name)))
    (unless (superchat-plugin-enabled-p name)
      (when-let* ((fn (plist-get definition :enable)))
        (funcall fn))
      (push name superchat-plugin--enabled))))

(defun superchat-plugin-disable (name)
  "Disable plugin NAME by calling its DISABLE, when it is enabled."
  (let ((definition (superchat-plugin--definition name)))
    (when (superchat-plugin-enabled-p name)
      (when-let* ((fn (plist-get definition :disable)))
        (funcall fn))
      (setq superchat-plugin--enabled (delq name superchat-plugin--enabled)))))

(defun superchat-plugin-enable-configured ()
  "Enable each plugin named in `superchat-plugins' that is not enabled."
  (dolist (name superchat-plugins)
    (superchat-plugin-enable name)))

(provide 'superchat-plugin)
;;; superchat-plugin.el ends here
