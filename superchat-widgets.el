;;; superchat-widgets.el --- Shared UI primitives for Superchat panels -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Shared UI primitives for Superchat panels.
;;
;; Side-window placement assigns each panel kind its own right-side slot, so
;; opening one panel does not evict another.
;;
;; Push-button elements use the explicit widget keymap required for reliable
;; activation and preserve caller-specific element properties.

;;; Code:

(require 'cl-lib)
(require 'widget)

;; ── Side-window placement ──

(defgroup superchat-side-window nil
  "Shared side-window placement policy for Superchat panels."
  :group 'superchat)

(defcustom superchat-side-window-width 0.42
  "Width of Superchat side windows as a fraction of the frame width."
  :type 'number
  :group 'superchat-side-window)

(defcustom superchat-side-window-slots
  '((plan . 0) (generated-ui . 1) (agents . 2))
  "Right side-window slot per Superchat panel kind.
The ordering of panels is a user preference."
  :type '(alist :key-type symbol :value-type integer)
  :group 'superchat-side-window)

(defun superchat-side-window-show (buffer kind)
  "Show BUFFER in the right side window assigned to panel KIND.
KIND must be registered in `superchat-side-window-slots'.  An unregistered
KIND is a programming error and signals an error instead of silently
defaulting to a shared slot."
  (let ((entry (assq kind superchat-side-window-slots)))
    (unless entry
      (error "Unregistered Superchat side-window kind: %S" kind))
    (when (not noninteractive)
      (dolist (win (get-buffer-window-list buffer nil t))
        (unless (or (eq (window-parameter win 'window-side) 'right)
                    (eq win (selected-window)))
          (delete-window win))))
    (display-buffer
     buffer
     `((display-buffer-in-side-window)
       (side . right)
       (slot . ,(cdr entry))
       (window-width . ,superchat-side-window-width)))))

;; ── Push-button elements ──

(defun superchat-widgets-button (tag notify &rest properties)
  "Return a TextUI push-button element with TAG that calls NOTIFY."
  (append (list :type 'push-button
                :tag tag
                :keymap widget-keymap
                :notify notify)
          properties))

(provide 'superchat-widgets)

;;; superchat-widgets.el ends here
