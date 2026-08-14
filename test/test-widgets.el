;;; test-widgets.el --- Tests for shared panel UI primitives -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-widgets)

(ert-deftest widgets/kinds-use-separate-slots ()
  "Different panel kinds receive different right-side slots."
  (let ((plan (generate-new-buffer "*side-window-plan-test*"))
        (generated-ui (generate-new-buffer "*side-window-generated-ui-test*"))
        calls)
    (unwind-protect
        (let ((superchat-side-window-slots
               '((plan . 0) (generated-ui . 1))))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buffer &optional action)
                       (push (cons buffer action) calls)
                       buffer)))
            (superchat-side-window-show plan 'plan)
            (superchat-side-window-show generated-ui 'generated-ui))
          (should (= 2 (length calls)))
          (let ((plan-action (cdr (assq plan calls)))
                (generated-action (cdr (assq generated-ui calls))))
            (should (= 0 (cdr (assq 'slot plan-action))))
            (should (= 1 (cdr (assq 'slot generated-action))))
            (should-not (equal (cdr (assq 'slot plan-action))
                               (cdr (assq 'slot generated-action))))))
      (when (buffer-live-p plan)
        (kill-buffer plan))
      (when (buffer-live-p generated-ui)
        (kill-buffer generated-ui)))))

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

;;; test-widgets.el ends here
