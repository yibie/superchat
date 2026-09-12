;;; superchat-transport-mock.el --- scripted model-transport for verification -*- lexical-binding: t; -*-

;;; Commentary:
;; A `model-transport' provider that answers from a script instead of a
;; network.  It is the no-test-suite verification tool (spec §5): mount it
;; in place of `transport-llm' and every Surface runs end to end with
;; deterministic answers.
;;
;;   (superchat-plugin-unmount 'transport-llm)
;;   (superchat-plugin-mount 'transport-mock
;;     '(:script ((:chunks ("par" "tial") :text "final answer")
;;                (:error "boom"))))
;;
;; Each request pops one step: `:chunks' are streamed first, then `:text'
;; completes the Run, `:error' fails it, `:hang' leaves it running (for
;; cancel cases).  An exhausted script answers "[mock script exhausted]".
;; `superchat-transport-mock-requests' records every prompt it received.

;;; Code:

(require 'superchat-plugin)
(require 'superchat-service)

(defvar superchat--llm-runtime-observer)

(defvar superchat-transport-mock-requests nil
  "Prompts received by the mock, newest first: (:prompt :system :tools :model).")

(defvar superchat-transport-mock--script nil)
(defvar superchat-transport-mock--timers nil)

(defun superchat-transport-mock-reset (&optional script)
  "Replace the pending SCRIPT and forget recorded requests."
  (dolist (timer superchat-transport-mock--timers)
    (when (timerp timer) (cancel-timer timer)))
  (setq superchat-transport-mock--timers nil
        superchat-transport-mock--script (copy-sequence script)
        superchat-transport-mock-requests nil))

(defun superchat-transport-mock--deliver (observer callback stream step)
  (dolist (chunk (plist-get step :chunks))
    (when stream (funcall stream chunk)))
  (cond
   ((plist-get step :hang) nil)
   ((plist-get step :error)
    (when (functionp observer)
      (funcall observer 'terminal (list :status 'failed
                                        :response (plist-get step :error))))
    (when callback (funcall callback (plist-get step :error))))
   (t
    (let ((text (or (plist-get step :text) "")))
      (when (functionp observer)
        (funcall observer 'terminal (list :status 'completed :response text)))
      (when callback (funcall callback text))))))

(defun superchat-transport-mock-generate
    (prompt callback stream-callback &optional target-model _context-files
            tools system-prompt _preset _decision)
  "`model-transport' `generate' honouring Runtime's observer protocol."
  (push (list :prompt prompt :system system-prompt :tools tools :model target-model)
        superchat-transport-mock-requests)
  (let* ((observer superchat--llm-runtime-observer)
         (step (or (pop superchat-transport-mock--script)
                   (list :text "[mock script exhausted]")))
         (handle (generate-new-buffer " *superchat-transport-mock*")))
    ;; Runtime installs the handle as the cancellable transport request.
    (when (functionp observer) (funcall observer 'request handle))
    (push (run-at-time (or (plist-get step :delay) 0.01) nil
                       #'superchat-transport-mock--deliver
                       observer callback stream-callback step)
          superchat-transport-mock--timers)
    handle))

(superchat-define-plugin transport-mock
  :doc "Scripted model-transport for verification."
  :config '((:script listp nil "Steps: (:chunks (…) :text S) | (:error S) | (:hang t)"))
  :apply
  (lambda (scope config)
    (superchat-transport-mock-reset (plist-get config :script))
    (superchat-scope-effect scope #'superchat-transport-mock-reset)
    (superchat-service-provide
     scope 'model-transport
     (list (cons 'generate #'superchat-transport-mock-generate)
           (cons 'capabilities (lambda (_model) (list :streaming t :tools t)))
           (cons 'context-budget (lambda (&rest _) 40000))))))

(provide 'superchat-transport-mock)
;;; superchat-transport-mock.el ends here
