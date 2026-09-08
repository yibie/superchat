;;; test-widgets.el --- Tests for shared panel UI primitives -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-genui)
(require 'superchat-plan)
(require 'superchat-widgets)

(ert-deftest widgets/inline-action-keymap-and-deferred-notify ()
  (with-temp-buffer
    (org-mode)
    (let (deferred called button)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn &rest args)
                   (setq deferred (lambda () (apply fn args))))))
        (setq button (superchat-widgets-inline-button "Answer" (lambda () (setq called t))))
        (goto-char (widget-get button :from))
        (should (eq (key-binding (kbd "RET")) #'widget-button-press))
        (should (eq (key-binding (kbd "<down-mouse-1>")) #'widget-button-click))
        (call-interactively (key-binding (kbd "RET")))
        (should-not called)
        (should (widget-at (point)))
        (should deferred)
        (funcall deferred)
        (should called)))))

(ert-deftest widgets/inline-mouse-command-defers-complete-action ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (org-mode)
      (let (deferred called)
        (let* ((button (superchat-widgets-inline-button "Stop response" (lambda () (setq called t))))
               (pos (marker-position (widget-get button :from)))
               (location (list (selected-window) pos '(0 . 0) 0))
               (down (list 'down-mouse-1 location))
               (up (list 'mouse-1 location)))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_time _repeat fn &rest args)
                       (setq deferred (lambda () (apply fn args)))))
                    ((symbol-function 'read--potential-mouse-event) (lambda () up)))
            (funcall (key-binding (kbd "<down-mouse-1>") nil t pos) down)
            (should-not called)
            (should (widget-at pos))
            (should deferred))
          (funcall deferred)
          (should called))))))

(defun widgets--right-side-windows ()
  "Return the live right-side windows on the selected frame."
  (cl-remove-if-not
   (lambda (window)
     (eq (window-parameter window 'window-side) 'right))
   (window-list (selected-frame) 'nomini)))

(ert-deftest widgets/real-side-windows-coexist-by-slot ()
  "Opening later panels does not evict earlier right-side panels."
  (let* ((frame (selected-frame))
         (original-width (frame-width frame))
         (configuration (current-window-configuration))
         (plan (generate-new-buffer "*widgets-real-plan*"))
         (generated-ui (generate-new-buffer "*widgets-real-generated-ui*"))
         (agents (generate-new-buffer "*widgets-real-agents*")))
    (unwind-protect
        (progn
          (set-frame-width frame 200)
          (should (= 0 (length (widgets--right-side-windows))))
          (superchat-side-window-show plan 'plan)
          (should (= 1 (length (widgets--right-side-windows))))
          (superchat-side-window-show generated-ui 'generated-ui)
          ;; The second panel must add a window, not evict the first one.
          (should (= 2 (length (widgets--right-side-windows))))
          (superchat-side-window-show agents 'agents)
          ;; The third panel must preserve both earlier windows as well.
          (should (= 3 (length (widgets--right-side-windows))))
          (let ((windows (widgets--right-side-windows))
                ;; `frame-width' stays stale for the batch terminal frame.
                (expected-width
                 (floor (* (frame-native-width frame)
                           superchat-side-window-width))))
            (dolist (entry (list (cons 0 plan)
                                 (cons 1 generated-ui)
                                 (cons 2 agents)))
              (let ((window (cl-find (cdr entry) windows
                                     :key #'window-buffer)))
                (should window)
                (should (eq 'right
                            (window-parameter window 'window-side)))
                (should (= (car entry)
                           (window-parameter window 'window-slot)))
                (should (eq (cdr entry) (window-buffer window)))
                (should (= expected-width (window-width window)))))))
      (set-window-configuration configuration)
      (set-frame-width frame original-width)
      (dolist (buffer (list plan generated-ui agents))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest widgets/panel-entry-points-use-separate-side-windows ()
  "Plan and generated-UI entry points land in distinct side windows."
  (let* ((frame (selected-frame))
         (original-width (frame-width frame))
         (configuration (current-window-configuration))
         (superchat-plan--view-buffer
          (generate-new-buffer-name "*widgets-entry-plan*"))
         (plan (generate-new-buffer superchat-plan--view-buffer))
         (generated-ui (generate-new-buffer "*widgets-entry-generated-ui*")))
    (unwind-protect
        (progn
          (set-frame-width frame 200)
          (should (= 0 (length (widgets--right-side-windows))))
          (superchat-plan--view-show)
          (superchat-genui--display generated-ui)
          (let ((plan-window (get-buffer-window plan frame))
                (generated-window (get-buffer-window generated-ui frame)))
            (should plan-window)
            (should generated-window)
            (should-not (eq plan-window generated-window))
            (should (eq 'right
                        (window-parameter plan-window 'window-side)))
            (should (eq 'right
                        (window-parameter generated-window 'window-side)))
            (should (= 0 (window-parameter plan-window 'window-slot)))
            (should (= 1 (window-parameter generated-window 'window-slot)))))
      (set-window-configuration configuration)
      (set-frame-width frame original-width)
      (dolist (buffer (list plan generated-ui))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest widgets/button-preserves-keymap-and-properties ()
  "The button builder sets the keymap and preserves extra properties."
  (let* ((notify (lambda () nil))
         (properties '(:panel-action :cancel-main :target-id "run-1"))
         (button (apply #'superchat-widgets-button
                        "Cancel" notify properties)))
    (should (eq 'push-button (plist-get button :type)))
    (should (equal "Cancel" (plist-get button :tag)))
    (should (eq widget-keymap (plist-get button :keymap)))
    (should (eq notify (plist-get button :notify)))
    (should (equal properties (nthcdr 8 button)))))

(ert-deftest widgets/input-panel-has-distinct-real-slot ()
  "The nonshipping input panel does not replace an existing Plan panel."
  (save-window-excursion
    (let ((plan (generate-new-buffer " *input-plan-slot*"))
          (input (generate-new-buffer " *input-question-slot*")))
      (unwind-protect
          (progn
            (superchat-side-window-show plan 'plan)
            (superchat-side-window-show input 'input)
            (let ((p (get-buffer-window plan)) (q (get-buffer-window input)))
              (should (window-live-p p))
              (should (window-live-p q))
              (should-not (eq p q))
              (should (= 3 (window-parameter q 'window-slot)))
              (should (eq 'right (window-parameter q 'window-side)))))
        (kill-buffer plan)
        (kill-buffer input)))))

;;; test-widgets.el ends here
