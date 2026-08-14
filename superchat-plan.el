;;; superchat-plan.el --- Structured plans: propose, review, approve -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A plan is a structured object, not prose.  The model proposes steps via
;; the `emit_plan' tool; the steps land on the Tape as `plan.proposed' and
;; render for review as stateful widget.el controls inline in the chat
;; buffer: a checkbox per step (the control carries the checked state),
;; whose value syncs into the plan data on every toggle.  The plan data is
;; the source of truth; approve reads the data, never rendered text.  The
;; review block is a one-time consumable: it renders after the proposing
;; Run finishes (so it lands after the assistant's text, before the next
;; prompt) and is deleted on approve or clear.
;;
;; TextUI is not used here: its regions require textui-mode, which the
;; chat buffer (org-mode) does not have.  widget.el controls, by contrast,
;; plant themselves in any buffer via overlays.
;;
;; Approving lands `plan.approved' on the Tape and the included steps
;; become the session's active plan, injected into every following
;; execution Run until cleared.  Acceptance criteria are commitments for
;; the human and future Closure checks — they never drive automatic
;; retries.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'superchat-tape)

(declare-function superchat-send-input "superchat")

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

(defvar superchat-plan--pending-render nil
  "Non-nil while a proposed plan waits for its inline review block.
Set by `superchat-plan-propose'; consumed by
`superchat-plan--render-inline' on turn completion so the review
lands after the assistant's final text.")

(defvar superchat-plan--block-start nil
  "Marker at the start of the inline review block, or nil.")

(defvar superchat-plan--block-end nil
  "Marker at the end of the inline review block, or nil.")

(defvar superchat-plan--count-marker nil
  "Marker at the inline \"N/M steps selected\" line, or nil.")

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

;; ── Inline review block: stateful widget.el controls in the chat buffer ──

(define-widget 'superchat-plan-step-checkbox 'checkbox
  "Plan review step: a checkbox whose value is the step's checked state.
The control itself carries the state; toggling it syncs the value into
the plan data.  :step-id links the control to its step."
  :format "%[%v%] %t"
  :value nil
  :notify #'superchat-plan--step-widget-notify)

(defun superchat-plan--step-widget-notify (widget &rest _)
  "Sync WIDGET's value (the step's checked state) into the plan data.
Runs in the chat buffer where the control lives; updates the inline
count line afterwards."
  (let ((id (widget-get widget :step-id)))
    (when (and id superchat-plan--proposed)
      (setf (plist-get
             (cl-find id (plist-get superchat-plan--proposed :steps)
                      :key (lambda (step) (plist-get step :id)))
             :checked)
            (widget-value widget))
      (superchat-plan--update-count))))

(defun superchat-plan--update-count ()
  "Rewrite the inline count line to match the current checked state."
  (let ((marker superchat-plan--count-marker))
    (when (and marker (marker-position marker))
      (with-current-buffer (marker-buffer marker)
        (let ((steps (and superchat-plan--proposed
                          (plist-get superchat-plan--proposed :steps))))
          (when steps
            (save-excursion
              (goto-char marker)
              (let ((inhibit-read-only t))
                (delete-region marker (line-end-position))
                (insert (format "%d/%d steps selected"
                                (cl-count t steps
                                          :key
                                          (lambda (s) (plist-get s :checked)))
                                (length steps)))))))))))

(defun superchat-plan--render-inline ()
  "Render the review block inline, after the assistant response.
Called from `superchat-turn-completed-functions'; inserts before the
next input prompt so the block follows the assistant's text and never
lands inside the input region."
  (when superchat-plan--pending-render
    (setq superchat-plan--pending-render nil)
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (when superchat-plan--proposed
        (let ((inhibit-read-only t)
              (steps (plist-get superchat-plan--proposed :steps))
              (proposed superchat-plan--proposed))
          (goto-char (point-max))
          (when (and (boundp 'superchat--prompt-start)
                     superchat--prompt-start
                     (marker-position superchat--prompt-start))
            ;; Insert BEFORE the "* User" headline line: inserting at the
            ;; marker itself would put the block inside the input region
            ;; and its text would be re-sent as the next request.
            (goto-char superchat--prompt-start)
            (beginning-of-line))
          (insert "\n")
          (setq superchat-plan--block-start (point-marker))
          (insert (format "** Plan: %s\n"
                          (or (plist-get proposed :summary) "plan")))
          (insert (format "%d/%d steps selected\n"
                          (cl-count t steps
                                    :key (lambda (s) (plist-get s :checked)))
                          (length steps)))
          (setq superchat-plan--count-marker (point-marker))
          (dolist (step steps)
            (widget-create 'superchat-plan-step-checkbox
                           :tag (format "%d. %s"
                                        (plist-get step :id)
                                        (plist-get step :text))
                           :value (plist-get step :checked)
                           :step-id (plist-get step :id))
            (insert "\n")
            (insert (format "     done when: %s\n"
                            (plist-get step :accept))))
          (widget-create 'push-button
                         :tag "Approve selected steps"
                         :notify #'superchat-plan--inline-approve)
          (insert "\n")
          (setq superchat-plan--block-end (point-marker)))))))

(defun superchat-plan--flush-on-terminal (status)
  "Render the pending inline review when STATUS is a terminal Run state."
  (when (fboundp 'superchat-runtime--terminal-p)
    (when (superchat-runtime--terminal-p (plist-get status :status))
      (superchat-plan--render-inline))))

(add-hook 'superchat-turn-completed-functions
          #'superchat-plan--flush-on-terminal)

(defun superchat-plan--clear-inline ()
  "Delete the inline review block (one-time consumable)."
  (when (and superchat-plan--block-start superchat-plan--block-end
             (marker-position superchat-plan--block-start)
             (marker-position superchat-plan--block-end))
    (with-current-buffer (marker-buffer superchat-plan--block-start)
      (let ((inhibit-read-only t))
        (delete-region superchat-plan--block-start
                       superchat-plan--block-end))))
  (setq superchat-plan--block-start nil
        superchat-plan--block-end nil
        superchat-plan--count-marker nil))

(defun superchat-plan--inline-approve (&rest _)
  "Approve the plan under review from the inline Approve button."
  (superchat-plan-approve))

;; ── Propose / approve / clear ──

(defun superchat-plan-propose (steps &optional summary)
  "Propose STEPS (a list of (text accept) pairs) as a reviewable plan.
Appends `plan.proposed' to the current session's Tape, stores the plan
as review data, and defers the inline review block to turn completion.
Returns a user-facing result string."
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
                                         :checked t))))
            (with-current-buffer (get-buffer-create superchat-buffer-name)
              (setq superchat-plan--proposed
                    (list :summary summary :event-id event-id
                          :steps structured)))
            (setq superchat-plan--pending-render t)
            (format
             (concat "Plan proposed: %d steps. Review the inline controls "
                     "after this reply: uncheck steps to drop, then press "
                     "Approve.")
             (length steps))))))))

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
        ;; Delete the consumed review block.  Deferred: approving from
        ;; the inline button runs inside the button widget's own command,
        ;; and deleting widget text under it is unsafe until it returns.
        (run-at-time 0 nil #'superchat-plan--clear-inline)))))

;;;###autoload
(defun superchat-plan-clear ()
  "Clear the session's active plan and any plan under review."
  (interactive)
  (setq superchat--active-plan nil
        superchat-plan--proposed nil)
  (superchat-plan--clear-inline)
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
