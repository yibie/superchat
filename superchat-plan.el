;;; superchat-plan.el --- Structured plans: propose, review, approve -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A plan is a structured object, not prose.  The model proposes steps via
;; the `emit_plan' tool; the steps land on the Tape as `plan.proposed' and
;; render for review as stateful widget.el controls (a checkbox per step,
;; whose value is the step's checked state) in a TextUI side window.  The
;; review never touches the chat buffer: the widget holds the state, the
;; plan data is the source of truth, and the rendered frame is a pure
;; projection of it.  Approving reads the data (never rendered text), lands
;; `plan.approved' on the Tape, and the included steps become the session's
;; active plan, injected into every following execution Run until cleared.
;; Acceptance criteria are commitments for the human and future Closure
;; checks — they never drive automatic retries.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'superchat-tape)

(declare-function superchat-send-input "superchat")
(declare-function textui-open "textui" (name render-function &optional initial-state))
(declare-function textui-update "textui" (buffer updater &rest keys))
(declare-function textui-request-refresh "textui" (buffer))

(defgroup superchat-plan nil
  "Structured plans: propose, review, approve."
  :group 'superchat)

(defcustom superchat-plan-auto-execute t
  "When non-nil, approving a plan with included steps immediately
sends a follow-up request telling the model to execute them."
  :type 'boolean
  :group 'superchat-plan)

(defvar-local superchat--active-plan nil
  "The approved plan for this session.
Shape: (:summary S :event-id E :steps ((:id N :text S :accept S
:included P) ...))")

(defvar-local superchat-plan--proposed nil
  "The plan currently under review in this buffer.
Shape: (:summary S :event-id E :steps ((:id N :text S :accept S
:checked P) ...))")

(defvar superchat-plan--view-buffer "*superchat-plan-review*"
  "TextUI buffer that renders the plan review as widget controls.")

(defvar superchat-plan--view-chat-buffer nil
  "Name of the chat buffer whose proposed plan the review view shows.
The view is a pure projection: the plan data lives in the chat buffer
and the widgets' state is synced back into it on every change.")

(defun superchat-plan--validate-steps (steps)
  "Return a rejection string for STEPS, or nil when they are valid.
Error messages carry a concrete example: the model that just failed
retries with the same shape unless the correction is pedagogical."
  (cond
   ((not (listp steps))
    "steps must be a list of (text accept) pairs, e.g. [[\"修改第二行\" \"第二行内容等于目标文本\"]]")
   ((null steps)
    "steps must not be empty")
   ((> (length steps) 20)
    "at most 20 steps")
   (t
    (cl-loop for step in steps
             for id from 1
             do (unless (listp step)
                  (cl-return
                   (format
                    (concat "step %d is not a list. Each step must be a "
                            "(text accept) PAIR: [\"what to do\" "
                            "\"how to verify it\"], "
                            "e.g. [\"修改第二行\" \"第二行内容等于目标文本\"]")
                    id)))
             do (unless (= 2 (length step))
                  (cl-return
                   (format
                    (concat "step %d has %d elements. Each step must be "
                            "a (text accept) PAIR of exactly two strings: "
                            "[\"what to do\" \"how to verify it\"], "
                            "e.g. [\"修改第二行\" \"第二行内容等于目标文本\"]")
                    id (length step))))
             do (unless (and (stringp (car step))
                             (not (string-empty-p (string-trim (car step)))))
                  (cl-return (format "step %d text must be a non-empty string"
                                    id)))
             do (unless (and (stringp (cadr step))
                             (not (string-empty-p (string-trim (cadr step)))))
                  (cl-return (format "step %d accept must be a non-empty string"
                                    id)))
             do (unless (<= (length (car step)) 500)
                  (cl-return (format "step %d text too long" id)))
             finally return nil))))

(defun superchat-plan--proposed-event (session-id steps summary)
  "Append the `plan.proposed' event for SESSION-ID and return its id."
  (plist-get
   (superchat-tape-append-event-record
    (list :type "plan.proposed" :schema 1
          :session-id session-id :actor-id "lead"
          :payload
          (list :summary summary
                :steps
                (cl-loop for step in steps
                         for id from 1
                         collect (list :id id :text (car step)
                                       :accept (cadr step))))))
   :event-id))

;; ── Review view: stateful widget controls in a TextUI side window ──

(define-widget 'superchat-plan-step-checkbox 'checkbox
  "Plan review step: a checkbox whose value is the step's checked state.
The control itself carries the state; toggling it syncs the value into
the plan data and refreshes the view.  :step-id links the control to
its step in the proposed plan."
  :format "%[%v%] %t"
  :value nil
  :notify #'superchat-plan--step-widget-notify)

(defun superchat-plan--step-widget-notify (widget &rest _)
  "Sync WIDGET's value (the step's checked state) into the plan data."
  (let* ((id (widget-get widget :step-id))
         (chat (and superchat-plan--view-chat-buffer
                    (get-buffer superchat-plan--view-chat-buffer))))
    (when (and id chat)
      (with-current-buffer chat
        (when superchat-plan--proposed
          (setf (plist-get
                 (cl-find id (plist-get superchat-plan--proposed :steps)
                          :key (lambda (step) (plist-get step :id)))
                 :checked)
                (widget-value widget)))))
    (when (and (fboundp 'textui-request-refresh)
               (derived-mode-p 'textui-mode))
      (textui-request-refresh (current-buffer)))))

(defun superchat-plan--view-frame (&optional _width)
  "Return the review frame for the proposed plan of the chat buffer."
  (let* ((chat (and superchat-plan--view-chat-buffer
                    (get-buffer superchat-plan--view-chat-buffer)))
         (proposed (and chat
                        (buffer-local-value 'superchat-plan--proposed chat))))
    (if (not proposed)
        (list (list :type 'text :value "No plan under review."))
      (let* ((steps (plist-get proposed :steps))
             (checked (cl-count t steps
                                :key (lambda (step)
                                       (plist-get step :checked)))))
        (append
         (list (list :type 'text
                     :value (format "** Plan: %s"
                                    (or (plist-get proposed :summary)
                                        "plan")))
               (list :type 'text
                     :value (format "%d/%d steps selected" checked
                                    (length steps))))
         (cl-loop for step in steps
                  append
                  (let ((id (plist-get step :id)))
                    (list
                     (list :type 'superchat-plan-step-checkbox
                           :tag (format "%d. %s" id
                                        (plist-get step :text))
                           :value (plist-get step :checked)
                           :step-id id
                           :layout
                           (list :focus-id
                                 (intern (format "plan-step-%d" id))))
                     (list :type 'text
                           :value (format "     done when: %s"
                                          (plist-get step :accept))))))
         (list (list :type 'push-button
                     :tag "Approve selected steps"
                     :notify #'superchat-plan--view-approve)))))))

(defun superchat-plan--view-available-p ()
  "Return non-nil when TextUI can render the review view."
  (and (or (featurep 'textui) (require 'textui nil t))
       (fboundp 'textui-open)
       (fboundp 'textui-update)
       (fboundp 'textui-request-refresh)))

(defun superchat-plan--view-update ()
  "Open or update the plan review side window."
  (let ((buffer (get-buffer superchat-plan--view-buffer)))
    (cond
     ((and buffer
           (with-current-buffer buffer (derived-mode-p 'textui-mode)))
      (textui-update buffer
                     (lambda (_old) (superchat-plan--view-frame)))
      (display-buffer
       buffer
       '((display-buffer-in-side-window display-buffer-reuse-window)
         (side . right)
         (window-width . 0.42)))
      buffer)
     (t
      (textui-open superchat-plan--view-buffer
                   (lambda (_width) (superchat-plan--view-frame)))))))

(defun superchat-plan--view-close ()
  "Close the review side window and clear its frame when it exists."
  (let ((buffer (get-buffer superchat-plan--view-buffer)))
    (when buffer
      (when-let ((win (get-buffer-window buffer)))
        (quit-window nil win))
      (when (and (fboundp 'textui-request-refresh)
                 (with-current-buffer buffer
                   (derived-mode-p 'textui-mode)))
        (textui-request-refresh buffer)))))

(defun superchat-plan--view-approve (&rest _)
  "Approve the plan under review from the review view."
  (if (and superchat-plan--view-chat-buffer
           (get-buffer superchat-plan--view-chat-buffer))
      (with-current-buffer (get-buffer superchat-plan--view-chat-buffer)
        (superchat-plan-approve))
    (message "No plan review to approve.")))

;; ── Propose / approve / clear ──

(defun superchat-plan-propose (steps &optional summary)
  "Propose STEPS (a list of (text accept) pairs) as a reviewable plan.
Appends `plan.proposed' to the current session's Tape, stores the plan
as review data, and renders the review side window when TextUI is
available.  Returns a user-facing result string."
  (let ((rejection (superchat-plan--validate-steps steps)))
    (if rejection
        (format "Error: %s" rejection)
      (if (not (and (boundp 'superchat--session-id) superchat--session-id))
          "Error: no active Superchat session"
        (if-let ((existing
                  (with-current-buffer
                      (get-buffer-create superchat-buffer-name)
                    (cond (superchat-plan--proposed 'under-review)
                          (superchat--active-plan 'approved)))))
            ;; One session, one plan: never open a second review.
            ;; "declined" classifies the message as non-retryable in the
            ;; tool error feedback, so the model stops proposing.
            (concat "Error: plan proposal declined — a plan is already "
                    (if (eq existing 'approved) "approved" "under review")
                    " in this session. Do not propose another plan.")
          (let* ((summary (string-trim (or summary "")))
                 (event-id
                  (superchat-plan--proposed-event
                   superchat--session-id steps summary))
                 (structured
                  (cl-loop for step in steps
                           for id from 1
                           collect (list :id id :text (car step)
                                         :accept (cadr step)
                                         ;; Default: keep all steps;
                                         ;; the user unchecks to drop.
                                         :checked t)))
                 (view-error nil))
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (setq superchat-plan--proposed
                    (list :summary summary :event-id event-id
                          :steps structured)))
            (setq superchat-plan--view-chat-buffer superchat-buffer-name)
            (when (superchat-plan--view-available-p)
              ;; The view is presentation only; a failure must not fail
              ;; the tool — the plan data and Tape event are the contract.
              (condition-case err
                  (superchat-plan--view-update)
                (error (setq view-error (error-message-string err)))))
            (cond
             (view-error
              (format "Plan proposed: %d steps (review view failed: %s)"
                      (length steps) view-error))
             ((superchat-plan--view-available-p)
              (format
               (concat "Plan proposed: %d steps. Review in the side "
                       "window: uncheck steps to drop, then Approve.")
               (length steps)))
             (t
              (format
               (concat "Plan proposed: %d steps (textui unavailable; "
                       "M-x superchat-plan-approve approves all steps).")
               (length steps))))))))))

;;;###autoload
(defun superchat-plan-approve ()
  "Approve the plan under review in the current session.
Checked steps become the active plan and are injected into every
following execution Run until `superchat-plan-clear'.  The checked
state is read from the plan data, never from rendered text."
  (interactive)
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (if (not superchat-plan--proposed)
        (message "No plan under review.")
      (let* ((all-steps (plist-get superchat-plan--proposed :steps))
             (included
              (mapcar (lambda (step) (plist-get step :id))
                      (cl-remove-if-not
                       (lambda (step) (plist-get step :checked))
                       all-steps)))
             (steps
              (mapcar
               (lambda (step)
                 (append step (list :included
                                    (and (plist-get step :checked) t))))
               all-steps))
             (event-id
              (plist-get
               (superchat-tape-append-event-record
                (list :type "plan.approved" :schema 1
                      :session-id superchat--session-id
                      :actor-id "user"
                      :payload
                      (list :proposed
                            (list :session-id superchat--session-id
                                  :event-id
                                  (plist-get superchat-plan--proposed
                                             :event-id))
                            :steps steps)))
               :event-id)))
        (setq superchat--active-plan
              (list :summary (plist-get superchat-plan--proposed :summary)
                    :event-id event-id :steps steps))
        (setq superchat-plan--proposed nil)
        ;; Approval is the planning→execution phase transition: exit the
        ;; planning identity so the execution Run is not governed by a
        ;; persona that forbids executing before approval.  The active
        ;; plan still travels with the Run via the prompt hook.
        (when (and (boundp 'superchat--active-preset)
                   superchat--active-preset
                   (fboundp 'superchat-preset-plan-p)
                   (superchat-preset-plan-p superchat--active-preset))
          (setq superchat--active-preset nil))
        (let ((auto (and included
                         superchat-plan-auto-execute
                         (boundp 'superchat--prompt-start)
                         superchat--prompt-start
                         (marker-position superchat--prompt-start))))
          (when auto
            ;; Approving authorizes execution; queue the follow-up into
            ;; the input region and send it so the Run starts now.  The
            ;; prompt hook injects the active plan into this request.
            (goto-char (point-max))
            (insert "请按已批准的计划开始执行。")
            (superchat-send-input))
          (message "Plan approved: %d/%d steps active%s"
                   (length included) (length steps)
                   (if auto " — executing" "")))
        ;; Close the review window last: quit-window may change the
        ;; selected window (and with it the current buffer), so nothing
        ;; after it may rely on the chat buffer being current.
        (superchat-plan--view-close)))))

;;;###autoload
(defun superchat-plan-clear ()
  "Clear the session's active plan and any plan under review."
  (interactive)
  (setq superchat--active-plan nil
        superchat-plan--proposed nil)
  (superchat-plan--view-close)
  (message "Active plan cleared."))

(defun superchat-plan--context-string (plan)
  "Return the prompt context block for the approved PLAN."
  (let ((included
         (cl-remove-if-not (lambda (step) (plist-get step :included))
                           (plist-get plan :steps))))
    (concat
     (format "Approved plan: %s\n" (or (plist-get plan :summary) "plan"))
     (mapconcat
      (lambda (step)
        (format "%d. %s\n   (done when: %s)"
                (plist-get step :id)
                (plist-get step :text)
                (plist-get step :accept)))
      included
      "\n")
     "\n\nThis plan is approved. Execute the included steps now. "
     "Do not propose a new plan.")))

(provide 'superchat-plan)

;;; superchat-plan.el ends here
